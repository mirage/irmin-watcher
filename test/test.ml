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
  let watcher = Irmin_watcher.v ~sw in
  let unwatch =
    Irmin_watcher.hook watcher 0 tmpdir (fun e ->
        events := e :: !events;
        Condition.broadcast cond)
  in
  let reset () = events := [] in
  let rec wait ?n () =
    Eio.Time.with_timeout_exn clock 360. (fun () ->
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
      let events = wait ~n () in
      let n =
        List.fold_left
          (fun n event ->
            match Hashtbl.find_opt expected event with
            | Some true ->
                Hashtbl.replace expected event false;
                n - 1
            | Some false -> n
            | None ->
                Fmt.pr "event: %s@." event;
                Alcotest.check Alcotest.reject s () ();
                assert false)
          n events
      in
      if n > 0 then loop n
    in
    loop n;
    Hashtbl.iter
      (fun k r ->
        Fmt.pr "%s %b@." k r;
        Alcotest.check Alcotest.bool s false r)
      expected
  in

  Hashtbl.replace expected "foo" true;
  write ~sw ~fs "foo" ("foo" ^ string_of_int i);
  wait_and_check_events "update foo";

  Hashtbl.replace expected "foo" true;
  remove ~fs "foo";
  wait_and_check_events "remove foo";

  Hashtbl.replace expected "foo" true;
  write ~sw ~fs "foo" ("foo" ^ string_of_int i);
  wait_and_check_events "create foo";

  Hashtbl.replace expected "bar" true;
  write ~sw ~fs "bar" ("bar" ^ string_of_int i);
  wait_and_check_events "create bar";

  Hashtbl.replace expected "bar" true;
  Hashtbl.replace expected "barx" true;
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

let check_stats watcher msg (watchdogs, dispatches) =
  let stats = Irmin_watcher.stats watcher in
  Alcotest.(check (pair int int))
    msg (watchdogs, dispatches)
    (stats.watchdogs, stats.dispatches)

let stats ~fs () =
  Eio.Switch.run @@ fun sw ->
  let tmpdir = tmpdir fs in
  mkdir tmpdir;
  let watcher = Irmin_watcher.v ~sw in
  let check = check_stats watcher in
  check "no hook" (0, 0);
  let unwatch_0 = Irmin_watcher.hook watcher 0 tmpdir ignore in
  check "one hook" (1, 1);
  let unwatch_1 = Irmin_watcher.hook watcher 1 tmpdir ignore in
  check "two hooks on the same directory" (1, 2);
  unwatch_0 ();
  check "one hook removed" (1, 1);
  unwatch_1 ();
  check "all hooks removed" (0, 0)

(* The watcher keeps working once the last hook on a directory is removed,
   including when two hooks on it start concurrently. *)
let unwatch ~fs ~clock () =
  Eio.Switch.run @@ fun sw ->
  let tmpdir = tmpdir fs in
  mkdir tmpdir;
  let watcher = Irmin_watcher.v ~sw in
  let unwatch = Irmin_watcher.hook watcher 0 tmpdir ignore in
  unwatch ();
  Eio.Time.sleep clock 0.2;
  let unwatch_0, unwatch_1 =
    Fiber.pair
      (fun () -> Irmin_watcher.hook watcher 0 tmpdir ignore)
      (fun () -> Irmin_watcher.hook watcher 1 tmpdir ignore)
  in
  unwatch_0 ();
  unwatch_1 ();
  Eio.Time.sleep clock 0.2;
  check_stats watcher "no watchdog left" (0, 0)

(* Removing a hook twice does not remove a later hook with the same id, and a
   hook that fails to start leaves nothing behind. *)
let cleanup ~fs () =
  let tmpdir = tmpdir fs in
  mkdir tmpdir;
  Eio.Switch.run (fun sw ->
      let watcher = Irmin_watcher.v ~sw in
      let unwatch_0 = Irmin_watcher.hook watcher 0 tmpdir ignore in
      unwatch_0 ();
      let unwatch_1 = Irmin_watcher.hook watcher 0 tmpdir ignore in
      unwatch_0 ();
      check_stats watcher "hook removed twice" (1, 1);
      unwatch_1 ());
  let watcher = Eio.Switch.run (fun sw -> Irmin_watcher.v ~sw) in
  (match Irmin_watcher.hook watcher 0 tmpdir ignore with
  | (_ : unit -> unit) ->
      Alcotest.fail "hook should fail once the switch is finished"
  | exception Invalid_argument _ -> ());
  check_stats watcher "failed hook" (0, 0)

(* A failing callback stops neither the watcher nor the other callbacks. *)
let failing_callback ~fs ~clock () =
  Eio.Switch.run @@ fun sw ->
  let tmpdir = tmpdir fs in
  mkdir tmpdir;
  let watcher = Irmin_watcher.v ~sw in
  let events = Eio.Stream.create max_int in
  let take () =
    Eio.Time.with_timeout_exn clock 60. (fun () -> Eio.Stream.take events)
  in
  let unwatch_0 =
    Irmin_watcher.hook watcher 0 tmpdir (fun _ -> failwith "callback failure")
  in
  let unwatch_1 = Irmin_watcher.hook watcher 1 tmpdir (Eio.Stream.add events) in
  write ~sw ~fs "foo" "foo";
  Alcotest.(check string) "first change" "foo" (take ());
  write ~sw ~fs "bar" "bar";
  Alcotest.(check string) "second change" "bar" (take ());
  unwatch_0 ();
  unwatch_1 ()

(* Callbacks receive paths relative to the watched directory, which can itself
   be relative to the current directory. *)
let relative ~cwd ~clock () =
  Eio.Switch.run @@ fun sw ->
  let dir = Eio.Path.(cwd / "irmin-watcher-relative") in
  Eio.Path.rmtree ~missing_ok:true dir;
  mkdir dir;
  let watcher = Irmin_watcher.v ~sw in
  let events = Eio.Stream.create max_int in
  let unwatch = Irmin_watcher.hook watcher 0 dir (Eio.Stream.add events) in
  Eio.Path.save ~create:(`Or_truncate 0o600) Eio.Path.(dir / "foo") "foo";
  let event =
    Eio.Time.with_timeout_exn clock 60. (fun () -> Eio.Stream.take events)
  in
  Alcotest.(check string) "path relative to the directory" "foo" event;
  unwatch ();
  Eio.Path.rmtree dir

let polling_tests ~sw ~fs ~cwd ~clock =
  [
    ("stats", `Quick, run ~fs (stats ~fs));
    ("unwatch", `Quick, run ~fs (unwatch ~fs ~clock));
    ("cleanup", `Quick, run ~fs (cleanup ~fs));
    ("failing callback", `Quick, run ~fs (failing_callback ~fs ~clock));
    ("relative", `Quick, relative ~cwd ~clock);
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

let tests ~sw ~fs ~cwd ~clock = [ (mode, polling_tests ~sw ~fs ~cwd ~clock) ]

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
  Alcotest.run "irmin-watch"
    (tests ~sw ~fs:env#fs ~cwd:env#cwd ~clock:env#clock)
