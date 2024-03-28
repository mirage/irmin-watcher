open Eio

let ( / ) = Filename.concat

let tmpdir = Filename.get_temp_dir_name () / "irmin-watcher"

let clean () =
  if Sys.file_exists tmpdir then
    let _ = Sys.command (Printf.sprintf "rm -rf '%s'" tmpdir) in
    ()

let run f () =
  clean ();
  f ()

let rec mkdir d =
  let perm = 0o0700 in
  try Unix.mkdir d perm with
  | Unix.Unix_error (Unix.EEXIST, "mkdir", _) -> ()
  | Unix.Unix_error (Unix.ENOENT, "mkdir", _) ->
      mkdir (Filename.dirname d);
      Unix.mkdir d perm

let write f d =
  let f = tmpdir / f in
  mkdir (Filename.dirname f);
  let oc = open_out f in
  output_string oc d;
  close_out oc

let move a b = Unix.rename (tmpdir / a) (tmpdir / b)

let remove f =
  try Unix.unlink (tmpdir / f) with e -> Alcotest.fail (Printexc.to_string e)

let poll ~mkdir:m i () =
  Eio.Switch.run @@ fun sw -> 
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
    match !events with
    | [] -> Condition.await_no_mutex cond; wait ?n ()
    | e -> (
        match n with
        | None ->
            reset ();
            e
        | Some n ->
            if List.length e < n then begin
              Condition.await_no_mutex cond;
              wait ~n ()
            end
            else (
              reset (); e))
  in

  write "foo" ("foo" ^ string_of_int i);
  let events = wait () in
  Alcotest.(check (slist string String.compare)) "updte foo" [ "foo" ] events;

  remove "foo";
  let events = wait () in
  Alcotest.(check (slist string String.compare)) "remove foo" [ "foo" ] events;

  write "foo" ("foo" ^ string_of_int i);
  let events = wait () in
  Alcotest.(check (slist string String.compare)) "create foo" [ "foo" ] events;

  write "bar" ("bar" ^ string_of_int i);
  let events = wait () in
  Alcotest.(check (slist string String.compare)) "create bar" [ "bar" ] events;

  move "bar" "barx";
  let events = wait ~n:2 () in
  Alcotest.(check (slist string String.compare))
    "move bar" [ "bar"; "barx" ] events;

  unwatch ()

let random_letter () = Char.(chr @@ (code 'a' + Random.int 26))

let rec random_filename () =
  Bytes.init (1 + Random.int 20) (fun _ -> random_letter ()) |> Bytes.to_string
  |> fun x -> if x = "foo" || x = "bar" then random_filename () else x

let random_path n =
  let rec aux = function 0 -> [] | n -> random_filename () :: aux (n - 1) in
  String.concat "/" (aux (n + 1))

let prepare_fs n =
  let fs = Array.init n (fun i -> (random_path 4, string_of_int i)) in
  Array.iter (fun (k, v) -> write k v) fs

let random_polls n () =
  mkdir tmpdir;
  let rec aux = function
    | 0 -> ()
    | i -> poll ~mkdir:false i (); aux (i - 1)
  in
  prepare_fs n;
  match Irmin_watcher.mode with `Polling -> aux 10 | _ -> aux 100

let polling_tests =
  [
    ("enoent", `Quick, run (poll ~mkdir:false 0));
    ("basic", `Quick, run (poll ~mkdir:true 0));
    ("100s", `Quick, run (random_polls 100));
    ("1000s", `Slow, run (random_polls 1000));
  ]

let mode =
  match Irmin_watcher.mode with
  | `FSEvents -> "fsevents"
  | `Inotify -> "inotify"
  | `Polling -> "polling"

let tests = [ (mode, polling_tests) ]

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
  Eio_main.run @@ fun _env ->
  Logs.set_level (Some Logs.Debug);
  Logs.set_reporter (reporter ());
  Irmin_watcher.set_polling_time 0.1;
  Alcotest.run "irmin-watch" tests
