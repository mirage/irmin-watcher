open Eio

let ( / ) = Filename.concat
let tmpdir fs = Eio.Path.(fs / Filename.get_temp_dir_name () / "irmin-watcher")
let clean fs = Eio.Path.rmtree ~missing_ok:true (tmpdir fs)

let run ~fs f () =
  clean fs;
  f ()

let mkdir d =
  let perm = 0o0700 in
  Eio.Path.mkdirs ~exists_ok:true ~perm d

let write ~sw ~fs f d =
  let f = Eio.Path.(tmpdir fs / f) in
  mkdir (fst @@ Option.get @@ Eio.Path.split f);
  let oc = Eio.Path.open_out ~sw ~create:(`Or_truncate 0o0700) f in
  Eio.Flow.copy_string d oc;
  Eio.Flow.close oc

let move ~fs a b = Eio.Path.(rename (tmpdir fs / a) (tmpdir fs / b))

let remove ~fs f =
  try Eio.Path.unlink Eio.Path.(tmpdir fs / f)
  with e -> Alcotest.fail (Printexc.to_string e)

let poll ~fs ~clock ~mkdir:m i () =
  Eio.Switch.run @@ fun sw ->
  let tmpdir = tmpdir fs in
  if m then mkdir tmpdir;
  let events = ref [] in
  let cond = Condition.create () in
  let unwatch =
    Irmin_watcher.hook ~sw 0 tmpdir (fun e ->
        events := e :: !events;
        Condition.broadcast cond)
  in
  let reset () = events := [] in
  let rec wait ?n () =
    Eio.Time.with_timeout_exn clock 60. (fun () ->
        match !events with
        | [] ->
            Condition.await_no_mutex cond;
            wait ?n ()
        | e -> (
            match n with
            | None ->
                reset ();
                e
            | Some n ->
                if List.length e < n then (
                  Condition.await_no_mutex cond;
                  wait ~n ())
                else (
                  reset ();
                  e)))
  in

  let expected = Hashtbl.create 10 in
  let wait_and_check_events ?(n = 1) s =
    let rec loop n =
      let events = List.map Eio.Path.native_exn (wait ~n ()) in
      let n =
        List.fold_left
          (fun n event ->
            match Hashtbl.find_opt expected event with
            | Some true ->
                Fmt.pr "BOOP@.";
                Hashtbl.replace expected event false;
                n
            | Some false -> n + 1
            | None ->
                Alcotest.check Alcotest.reject s () ();
                assert false)
          0 events
      in
      if n > 0 then loop n
    in
    loop n;
    Hashtbl.iter (fun _ r -> Alcotest.check Alcotest.bool s false r) expected
  in

  Hashtbl.replace expected Eio.Path.(native_exn @@ (tmpdir / "foo")) true;
  write ~sw ~fs "foo" ("foo" ^ string_of_int i);
  wait_and_check_events "update foo";

  Hashtbl.replace expected Eio.Path.(native_exn @@ (tmpdir / "foo")) true;
  remove ~fs "foo";
  wait_and_check_events "remove foo";

  Hashtbl.replace expected Eio.Path.(native_exn @@ (tmpdir / "foo")) true;
  write ~sw ~fs "foo" ("foo" ^ string_of_int i);
  wait_and_check_events "create foo";

  Hashtbl.replace expected Eio.Path.(native_exn @@ (tmpdir / "bar")) true;
  write ~sw ~fs "bar" ("bar" ^ string_of_int i);
  wait_and_check_events "create bar";

  Hashtbl.replace expected Eio.Path.(native_exn @@ (tmpdir / "bar")) true;
  Hashtbl.replace expected Eio.Path.(native_exn @@ (tmpdir / "barx")) true;
  move ~fs "bar" "barx";
  wait_and_check_events ~n:2 "move bar";
  unwatch ()

let random_letter () = Char.(chr @@ (code 'a' + Random.int 26))

let rec random_filename () =
  Bytes.init (1 + Random.int 20) (fun _ -> random_letter ()) |> Bytes.to_string
  |> fun x -> if x = "foo" || x = "bar" then random_filename () else x

let random_path n =
  let rec aux = function 0 -> [] | n -> random_filename () :: aux (n - 1) in
  String.concat "/" (aux (n + 1))

let prepare_fs ~sw ~fs:eio_fs n =
  let fs = Array.init n (fun i -> (random_path 4, string_of_int i)) in
  Array.iter (fun (k, v) -> write ~sw ~fs:eio_fs k v) fs

let random_polls ~sw ~fs ~clock n () =
  mkdir (tmpdir fs);
  let rec aux = function
    | 0 -> ()
    | i ->
        poll ~fs ~clock ~mkdir:false i ();
        aux (i - 1)
  in
  prepare_fs ~sw ~fs n;
  match Irmin_watcher.mode with `Polling -> aux 10 | _ -> aux 100

let polling_tests ~sw ~fs ~clock =
  [
    ("enoent", `Quick, run ~fs (poll ~fs ~clock ~mkdir:false 0));
    ("basic", `Quick, run ~fs (poll ~fs ~clock ~mkdir:true 0));
    ("100s", `Quick, run ~fs (random_polls ~sw ~fs ~clock 100));
    ("1000s", `Slow, run ~fs (random_polls ~sw ~fs ~clock 1000));
  ]

let mode =
  match Irmin_watcher.mode with
  | `FSEvents -> "fsevents"
  | `Inotify -> "inotify"
  | `Polling -> "polling"

let tests ~sw ~fs ~clock = [ (mode, polling_tests ~sw ~fs ~clock) ]

let reporter () =
  let pad n x =
    if String.length x > n then x
    else x ^ Astring.String.v ~len:(n - String.length x) (fun _ -> ' ')
  in
  let report src level ~over k msgf =
    let k _ =
      over ();
      k ()
    in
    let ppf = match level with Logs.App -> Fmt.stdout | _ -> Fmt.stderr in
    let with_stamp h _tags k fmt =
      let dt = Mtime.Span.to_float_ns (Mtime_clock.elapsed ()) in
      Fmt.kpf k ppf
        ("%+04.0fus %a %a @[" ^^ fmt ^^ "@]@.")
        dt
        Fmt.(styled `Magenta string)
        (pad 10 @@ Logs.Src.name src)
        Logs_fmt.pp_header (level, h)
    in
    msgf @@ fun ?header ?tags fmt -> with_stamp header tags k fmt
  in
  { Logs.report }

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  Lwt_eio.with_event_loop ~clock:env#clock @@ fun _ ->
  Logs.set_level (Some Logs.Debug);
  Logs.set_reporter (reporter ());
  Irmin_watcher.set_polling_time 0.1;
  Alcotest.run "irmin-watch" (tests ~sw ~fs:env#fs ~clock:env#clock)
