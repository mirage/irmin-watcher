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

let basic_polling () =
  let events = ref [] in
  let c = Lwt_condition.create () in
  Irmin_watcher.hook 0 tmpdir (fun x ->
      events := x :: !events;
      Lwt_condition.broadcast c x;
      Lwt.return_unit
    ) >>= fun u ->
  let foo = tmpdir / "foo" in
  let bar = tmpdir / "bar" in
  let write f d =
    let oc = open_out f in
    output_string oc d;
    close_out oc
  in
  write foo "foo";
  Lwt_condition.wait c >>= fun _ ->
  Alcotest.(check (slist string String.compare)) "foo" ["foo"] !events;
  write bar "bar";
  Lwt_condition.wait c >>= fun _ ->
  Alcotest.(check (slist string String.compare)) "bar" ["foo";"bar"] !events;
  u ();
  Lwt.return_unit

let polling_tests = [
  "basic", `Quick, run basic_polling;
]

let tests = [
  "polling", polling_tests;
]

let () =
  Logs.set_level (Some Logs.Debug);
  Logs.set_reporter (Logs_fmt.reporter ());
  Alcotest.run "irmin-watch" tests
