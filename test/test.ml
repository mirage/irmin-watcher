open Lwt.Infix

let (/) = Filename.concat

let tmpdir = Filename.get_temp_dir_name () / "irmin-watcher"

let clean () =
  if Sys.file_exists tmpdir then (
    let _ = Sys.command (Printf.sprintf "rm -rf '%s'" tmpdir) in
    ()
  );
  Unix.mkdir tmpdir 0o755

let run f () =
  clean ();
  Lwt_main.run (f ())

let rec mkdir d =
  let perm = 0o0700 in
  try Unix.mkdir d perm
  with
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

let poll ~fs i () =
  let events = ref [] in
  let cond = Lwt_condition.create () in
  Array.iter (fun (k, v) -> write k v) fs;
  Irmin_watcher.hook >>= fun hook ->
  hook 0 tmpdir (fun e ->
      events := e :: !events;
      Lwt_condition.broadcast cond ();
      Lwt.return_unit
    ) >>= fun unwatch ->
  let reset () = events := [] in
  let rec wait () = match !events with
  | [] -> Lwt_condition.wait cond >>= wait
  | e  -> Lwt.return e
  in
  reset ();
  write "foo" ("foo" ^ string_of_int i);
  wait () >>= fun events ->
  Alcotest.(check (slist string String.compare)) "foo" ["foo"] events;
  reset ();
  write "bar" ("bar" ^ string_of_int i);
  wait () >>= fun events ->
  Alcotest.(check (slist string String.compare)) "bar" ["bar"] events;
  unwatch ()

let random_letter () = Char.(chr @@ code 'a' + Random.int 26)

let rec random_filename () =
  Bytes.init (1 + Random.int 20) (fun _ -> random_letter ())
  |> Bytes.to_string
  |> fun x -> if x = "foo" || x = "bar" then random_filename () else x

let random_path n =
  let rec aux = function
  | 0 -> []
  | n -> random_filename () :: aux (n-1)
  in
  String.concat "/" (aux (n+1))

let random_polls () =
  let rec aux = function
  | 0 -> Lwt.return_unit
  | i ->
      let fs = Array.init 1000 (fun i -> random_path 4, string_of_int i) in
      poll ~fs i () >>= fun () ->
      aux (i-1)
  in
  aux 10

let polling_tests = [
  "basic", `Quick, run (poll ~fs:[||] 0);
  "lots" , `Quick, run random_polls;
]

let mode = match Irmin_watcher.mode with
| `FSEvents -> "fsevents"
| `Inotify  -> "inotify"
| `Polling  -> "polling"

let tests = [
  mode, polling_tests;
]

let () =
  Logs.set_level (Some Logs.Debug);
  Logs.set_reporter (Logs_fmt.reporter ());
  Alcotest.run "irmin-watch" tests
