(*---------------------------------------------------------------------------
   Copyright (c) 2016 Thomas Gazagnaire. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Lwt.Infix

let src = Logs.Src.create "irw-fsevents" ~doc:"Irmin watcher using FSevents"

module Log = (val Logs.src_log src : Logs.LOG)

let create_flags = Fsevents.CreateFlags.detailed_interactive
let run_loop_mode = Cf.RunLoop.Mode.Default

(* [release watcher ~started runloop] frees the stream [watcher] and stops its
   run loop. A started stream may still be delivering events through the Lwt
   event loop, so it is freed once they are delivered. Without a running Lwt
   event loop, the stream leaks rather than blocking the caller. *)
let release watcher ~started runloop =
  let free () =
    Fsevents_lwt.invalidate watcher;
    Fsevents_lwt.release watcher;
    Cf.RunLoop.stop runloop
  in
  if not started then free ()
  else (
    Fsevents_lwt.stop watcher;
    Lwt.async (fun () ->
        Lwt.catch
          (fun () -> Fsevents_lwt.flush watcher >|= free)
          (fun ex ->
            Log.err (fun l ->
                l "cannot release the FSEvents stream: %a" Fmt.exn ex);
            Lwt.return_unit)))

let start_runloop ~sw dir =
  let dir = Eio.Path.native_exn dir in
  (* FSEvents needs absolute paths. *)
  let dir =
    if Filename.is_relative dir then Filename.concat (Sys.getcwd ()) dir
    else dir
  in
  Log.debug (fun l -> l "start_runloop %s" dir);
  let watcher = Fsevents_lwt.create 0. create_flags [ dir ] in
  let stream = Fsevents_lwt.stream watcher in
  let event_stream = Fsevents_lwt.event_stream watcher in
  (* The run loop thread only records whether the stream started: the result
     is read on the Eio side. *)
  let start = Atomic.make false in
  let scheduler =
    Cf_lwt.RunLoop.run_thread (fun runloop ->
        Fsevents.schedule_with_run_loop event_stream runloop run_loop_mode;
        Atomic.set start (Fsevents.start event_stream))
  in
  match Lwt_eio.Promise.await_lwt scheduler with
  | runloop ->
      let started = Atomic.get start in
      Eio.Switch.on_release sw (fun () -> release watcher ~started runloop);
      if not started then
        Fmt.failwith "Irmin_watcher: cannot start the FSEvents stream for %s"
          dir;
      stream
  | exception (Eio.Cancel.Cancelled _ as e) ->
      let bt = Printexc.get_raw_backtrace () in
      (* The run loop thread is still starting the stream: release it once
         the thread is done. *)
      Lwt.on_success scheduler (fun runloop ->
          release watcher ~started:(Atomic.get start) runloop);
      Printexc.raise_with_backtrace e bt

let listen dir stream fn =
  let path_of_event { Fsevents_lwt.path; _ } = path in
  let iter () =
    Lwt_stream.iter_s
      (fun e ->
        let path = Eio.Path.(dir / path_of_event e) in
        Log.debug (fun l -> l "fsevents: %a" Eio.Path.pp path);
        fn @@ path)
      stream
  in
  Lwt_eio.run_lwt iter

(* Note: we use FSevents to detect any change, and we re-read the full
   tree on every change (so very similar to active polling, but
   blocking on incoming FSevents instead of sleeping). We could
   probably do better, but at the moment it is more robust to do so,
   to avoid possible duplicated events. *)
let v ~sw ~polling_time:_ =
  let listen dir f =
    Log.info (fun l -> l "FSevents mode");
    Core.spawn ~sw (fun ~sw ~ready ->
        (* Every change triggers a full scan: pending changes are merged. *)
        let pending = ref false in
        let cond = Lwt_condition.create () in
        let stream = start_runloop ~sw dir in
        let rec wait_for_changes () =
          if !pending then (
            pending := false;
            Lwt.return `Unknown)
          else Lwt_condition.wait cond >>= wait_for_changes
        in
        let wait_for_changes () = Lwt_eio.run_lwt wait_for_changes in
        Eio.Fiber.both
          (fun () ->
            listen dir stream (fun _path ->
                pending := true;
                Lwt_condition.signal cond ();
                Lwt.return_unit))
          (fun () -> Hook.run ~ready ~wait_for_changes ~dir f))
  in
  Core.create ~sw listen

let mode = `FSEvents

(*---------------------------------------------------------------------------
   Copyright (c) 2016 Thomas Gazagnaire

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
