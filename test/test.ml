open Eio

let ( / ) = Filename.concat
let tmpdir fs = Eio.Path.(fs / Filename.get_temp_dir_name () / "irmin-watcher")
let clean fs = Eio.Path.rmtree ~missing_ok:true (tmpdir fs)

let run ~fs f () =
  clean fs;
  f ()

let watcher ~sw = Irmin_watcher.v ~polling_time:0.1 ~sw ()

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
  let watcher = watcher ~sw in
  let unwatch =
    Irmin_watcher.hook watcher 0 tmpdir (fun e ->
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

(* [wait_for_all ~clock events names] takes the elements of [events] until it
   has seen all of [names]: writing a file can report it more than once. *)
let wait_for_all ~clock events names =
  Eio.Time.with_timeout_exn clock 60. (fun () ->
      let rec loop = function
        | [] -> ()
        | names ->
            let name = Eio.Stream.take events in
            loop (List.filter (( <> ) name) names)
      in
      loop names)

let wait_for ~clock events name = wait_for_all ~clock events [ name ]

let drain events =
  List.init (Eio.Stream.length events) (fun _ -> Eio.Stream.take events)

(* The number of open file descriptors, where it is known. *)
let open_fds () =
  match Sys.readdir "/proc/self/fd" with
  | fds -> Some (Array.length fds)
  | exception Sys_error _ -> None

let stats ~fs () =
  Eio.Switch.run @@ fun sw ->
  let tmpdir = tmpdir fs in
  mkdir tmpdir;
  let watcher = watcher ~sw in
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
  let watcher = watcher ~sw in
  let unwatch = Irmin_watcher.hook watcher 0 tmpdir ignore in
  unwatch ();
  Eio.Time.sleep clock 0.2;
  let events = Eio.Stream.create max_int in
  let unwatch_0, unwatch_1 =
    Fiber.pair
      (fun () -> Irmin_watcher.hook watcher 0 tmpdir (Eio.Stream.add events))
      (fun () -> Irmin_watcher.hook watcher 1 tmpdir ignore)
  in
  check_stats watcher "one watchdog" (1, 2);
  write ~sw ~fs "foo" "foo";
  wait_for ~clock events "foo";
  unwatch_0 ();
  unwatch_1 ();
  Eio.Time.sleep clock 0.2;
  check_stats watcher "no watchdog left" (0, 0)

(* Two hooks can start at the same time on a missing directory. *)
let missing_dir ~fs () =
  Eio.Switch.run @@ fun sw ->
  let dir = Eio.Path.(tmpdir fs / "missing") in
  let watcher = watcher ~sw in
  let unwatch_0, unwatch_1 =
    Fiber.pair
      (fun () -> Irmin_watcher.hook watcher 0 dir ignore)
      (fun () -> Irmin_watcher.hook watcher 1 dir ignore)
  in
  unwatch_0 ();
  unwatch_1 ()

(* Removing a hook twice does not remove a later hook with the same id, and a
   hook that fails to start leaves nothing behind. *)
let cleanup ~fs () =
  let tmpdir = tmpdir fs in
  mkdir tmpdir;
  Eio.Switch.run (fun sw ->
      let watcher = watcher ~sw in
      let unwatch_0 = Irmin_watcher.hook watcher 0 tmpdir ignore in
      unwatch_0 ();
      let unwatch_1 = Irmin_watcher.hook watcher 0 tmpdir ignore in
      unwatch_0 ();
      check_stats watcher "hook removed twice" (1, 1);
      unwatch_1 ());
  let watcher = Eio.Switch.run (fun sw -> watcher ~sw) in
  (match Irmin_watcher.hook watcher 0 tmpdir ignore with
  | (_ : unit -> unit) ->
      Alcotest.fail "hook should fail once the switch is finished"
  | exception Invalid_argument _ -> ());
  check_stats watcher "failed hook" (0, 0)

(* A hook whose caller is cancelled while the hook starts leaves no watchdog
   running. *)
let cancelled_hook ~fs ~clock () =
  Eio.Switch.run @@ fun sw ->
  let tmpdir = tmpdir fs in
  mkdir tmpdir;
  let watcher = watcher ~sw in
  let fds = open_fds () in
  Fiber.first
    (fun () ->
      ignore (Irmin_watcher.hook watcher 0 tmpdir ignore : unit -> unit))
    (fun () -> ());
  Eio.Time.sleep clock 0.2;
  check_stats watcher "no hook" (0, 0);
  Alcotest.(check (option int)) "no file descriptor left" fds (open_fds ())

(* [hook] fails, rather than blocking, when the switch of the watcher is
   cancelled but not finished yet, including on a directory that is already
   watched. No watchdog is left once the switch is finished. *)
let cancelled_switch ~fs ~clock () =
  let tmpdir = tmpdir fs in
  mkdir tmpdir;
  let hook watcher dir =
    Eio.Time.with_timeout_exn clock 10. (fun () ->
        match Irmin_watcher.hook watcher 1 dir ignore with
        | (_ : unit -> unit) -> "hooked"
        | exception Invalid_argument _ -> "Invalid_argument")
  in
  let created = ref None in
  let results, set_results = Promise.create () in
  Eio.Switch.run @@ fun outer ->
  (try
     Eio.Switch.run (fun sw ->
         let w = watcher ~sw in
         created := Some w;
         let (_ : unit -> unit) = Irmin_watcher.hook w 0 tmpdir ignore in
         Eio.Switch.fail sw Exit;
         Fiber.fork ~sw:outer (fun () ->
             let watched = hook w tmpdir in
             let other = hook w Eio.Path.(tmpdir / "other") in
             Promise.resolve set_results [ watched; other ]))
   with Exit -> ());
  (* Wait for the fiber, which can still be running. *)
  Alcotest.(check (list string))
    "hook raises Invalid_argument"
    [ "Invalid_argument"; "Invalid_argument" ]
    (Promise.await results);
  check_stats (Option.get !created) "no watchdog left" (0, 0)

(* Hooks on the same directory share a watchdog: they all receive the changes,
   and removing one of them does not stop the others. *)
let shared ~fs ~clock () =
  Eio.Switch.run @@ fun sw ->
  let tmpdir = tmpdir fs in
  mkdir tmpdir;
  let watcher = watcher ~sw in
  let events_0 = Eio.Stream.create max_int in
  let events_1 = Eio.Stream.create max_int in
  let unwatch_0 =
    Irmin_watcher.hook watcher 0 tmpdir (Eio.Stream.add events_0)
  in
  let unwatch_1 =
    Irmin_watcher.hook watcher 1 tmpdir (Eio.Stream.add events_1)
  in
  check_stats watcher "one watchdog" (1, 2);
  write ~sw ~fs "foo" "foo";
  wait_for ~clock events_0 "foo";
  wait_for ~clock events_1 "foo";
  unwatch_0 ();
  write ~sw ~fs "bar" "bar";
  wait_for ~clock events_1 "bar";
  Alcotest.(check bool)
    "removed hook not called" false
    (List.mem "bar" (drain events_0));
  unwatch_1 ()

(* A failing callback stops neither the watcher nor the other callbacks. *)
let failing_callback ~fs ~clock () =
  Eio.Switch.run @@ fun sw ->
  let tmpdir = tmpdir fs in
  mkdir tmpdir;
  let watcher = watcher ~sw in
  let events = Eio.Stream.create max_int in
  let unwatch_0 =
    Irmin_watcher.hook watcher 0 tmpdir (fun _ -> failwith "callback failure")
  in
  let unwatch_1 = Irmin_watcher.hook watcher 1 tmpdir (Eio.Stream.add events) in
  write ~sw ~fs "foo" "foo";
  wait_for ~clock events "foo";
  write ~sw ~fs "bar" "bar";
  wait_for ~clock events "bar";
  unwatch_0 ();
  unwatch_1 ()

(* Callbacks receive paths relative to the watched directory, which can itself
   be relative to the current directory. *)
let relative ~cwd ~clock () =
  Eio.Switch.run @@ fun sw ->
  let dir = Eio.Path.(cwd / "irmin-watcher-relative") in
  Eio.Path.rmtree ~missing_ok:true dir;
  mkdir dir;
  let watcher = watcher ~sw in
  let events = Eio.Stream.create max_int in
  let unwatch = Irmin_watcher.hook watcher 0 dir (Eio.Stream.add events) in
  Eio.Path.save ~create:(`Or_truncate 0o600) Eio.Path.(dir / "foo") "foo";
  let event =
    Eio.Time.with_timeout_exn clock 60. (fun () -> Eio.Stream.take events)
  in
  Alcotest.(check string) "path relative to the directory" "foo" event;
  unwatch ();
  Eio.Path.rmtree dir

(* A file that cannot be read neither makes [hook] fail nor hides the other
   changes, including those found by a full scan. *)
let unreadable ~fs ~clock () =
  Eio.Switch.run @@ fun sw ->
  let tmpdir = tmpdir fs in
  mkdir tmpdir;
  let secret = Eio.Path.(native_exn (tmpdir / "secret")) in
  write ~sw ~fs "secret" "secret";
  Unix.chmod secret 0o000;
  Fun.protect ~finally:(fun () -> Unix.chmod secret 0o600) @@ fun () ->
  let watcher = watcher ~sw in
  let events = Eio.Stream.create max_int in
  let unwatch = Irmin_watcher.hook watcher 0 tmpdir (Eio.Stream.add events) in
  (* With Inotify, creating a directory triggers a full scan. *)
  mkdir Eio.Path.(tmpdir / "d");
  write ~sw ~fs "top" "top";
  wait_for ~clock events "top";
  unwatch ()

(* A file that cannot be read when it changes is reported once it can be read
   again, even when another file changes in the meantime. *)
let pending_rescan ~fs ~clock () =
  Eio.Switch.run @@ fun sw ->
  let tmpdir = tmpdir fs in
  mkdir tmpdir;
  let secret = Eio.Path.(native_exn (tmpdir / "secret")) in
  write ~sw ~fs "secret" "A";
  Fun.protect ~finally:(fun () -> Unix.chmod secret 0o600) @@ fun () ->
  let watcher = watcher ~sw in
  let events = Eio.Stream.create max_int in
  let unwatch = Irmin_watcher.hook watcher 0 tmpdir (Eio.Stream.add events) in
  Unix.chmod secret 0o200;
  Out_channel.with_open_gen [ Open_wronly; Open_trunc ] 0o200 secret (fun oc ->
      Out_channel.output_string oc "B");
  write ~sw ~fs "top" "top";
  (* Wait for [top], noting whether [secret] was read anyway (as root). *)
  let seen_secret = ref false in
  Eio.Time.with_timeout_exn clock 60. (fun () ->
      let rec loop () =
        match Eio.Stream.take events with
        | "top" -> ()
        | "secret" ->
            seen_secret := true;
            loop ()
        | _ -> loop ()
      in
      loop ());
  Unix.chmod secret 0o600;
  if not !seen_secret then wait_for ~clock events "secret";
  unwatch ()

(* Symbolic links to directories are not followed, whether they lead back into
   the watched directory or out of it, but symbolic links to files are read.
   Loops of symbolic links are not reported as errors. *)
let symlinks ~fs ~clock () =
  Eio.Switch.run @@ fun sw ->
  let tmpdir = tmpdir fs in
  let dir = Eio.Path.(tmpdir / "w") in
  write ~sw ~fs "w/sub/foo" "foo";
  write ~sw ~fs "out/foo" "foo";
  let link target name =
    Unix.symlink target Eio.Path.(native_exn (tmpdir / name))
  in
  link "." "w/self";
  link ".." "w/sub/parent";
  link "../out" "w/out";
  link "loop" "w/loop";
  let watcher = watcher ~sw in
  let events = Eio.Stream.create max_int in
  let unwatch =
    Eio.Time.with_timeout_exn clock 10. (fun () ->
        Irmin_watcher.hook watcher 0 dir (Eio.Stream.add events))
  in
  write ~sw ~fs "out/bar" "bar";
  link "../out/foo" "w/file";
  (* With Inotify, creating a directory triggers a full scan. *)
  mkdir Eio.Path.(dir / "d");
  write ~sw ~fs "w/top" "top";
  (* Every event until both [file] and [top] are reported. *)
  let seen =
    Eio.Time.with_timeout_exn clock 60. (fun () ->
        let rec loop seen =
          if List.mem "file" seen && List.mem "top" seen then seen
          else loop (Eio.Stream.take events :: seen)
        in
        loop [])
  in
  Alcotest.(check (list string))
    "only the changed files" []
    (List.filter (fun e -> e <> "file" && e <> "top") (seen @ drain events));
  unwatch ()

(* Renaming a sub-directory reports the files under it, at their old and new
   paths. *)
let renamed_dir ~fs ~clock () =
  Eio.Switch.run @@ fun sw ->
  write ~sw ~fs "sub/foo" "foo";
  let watcher = watcher ~sw in
  let events = Eio.Stream.create max_int in
  let unwatch =
    Irmin_watcher.hook watcher 0 (tmpdir fs) (Eio.Stream.add events)
  in
  (* Wait for the first scan, so that the renaming is only seen through its
     events. *)
  write ~sw ~fs "bar" "bar";
  wait_for ~clock events "bar";
  move ~fs "sub" "sub2";
  wait_for_all ~clock events [ "sub/foo"; "sub2/foo" ];
  unwatch ()

let polling_time () =
  Eio.Switch.run @@ fun sw ->
  match Irmin_watcher.v ~polling_time:0. ~sw () with
  | (_ : Irmin_watcher.t) -> Alcotest.fail "v should reject a polling time of 0"
  | exception Invalid_argument _ -> ()

let polling_tests ~sw ~fs ~cwd ~clock =
  [
    ("stats", `Quick, run ~fs (stats ~fs));
    ("unwatch", `Quick, run ~fs (unwatch ~fs ~clock));
    ("missing directory", `Quick, run ~fs (missing_dir ~fs));
    ("cleanup", `Quick, run ~fs (cleanup ~fs));
    ("cancelled hook", `Quick, run ~fs (cancelled_hook ~fs ~clock));
    ("cancelled switch", `Quick, run ~fs (cancelled_switch ~fs ~clock));
    ("shared", `Quick, run ~fs (shared ~fs ~clock));
    ("failing callback", `Quick, run ~fs (failing_callback ~fs ~clock));
    ("relative", `Quick, relative ~cwd ~clock);
    ("unreadable file", `Quick, run ~fs (unreadable ~fs ~clock));
    ("pending rescan", `Quick, run ~fs (pending_rescan ~fs ~clock));
    ("symbolic links", `Quick, run ~fs (symlinks ~fs ~clock));
    ("renamed directory", `Quick, run ~fs (renamed_dir ~fs ~clock));
    ("polling time", `Quick, polling_time);
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
  Alcotest.run "irmin-watch"
    (tests ~sw ~fs:env#fs ~cwd:env#cwd ~clock:env#clock)
