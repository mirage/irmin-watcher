(*---------------------------------------------------------------------------
   Copyright (c) 2016 Thomas Gazagnaire. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Lwt.Infix

let src = Logs.Src.create "irw-inotify" ~doc:"Irmin watcher using Inotify"
module Logs = (val Logs.src_log src : Logs.LOG)

let listen dir fn =
  let path_of_event (_, _, _, p) = match p with None -> "" | Some p -> p in
  let rec iter i =
    Lwt_inotify.read i >>= fun e ->
    let path = path_of_event e in
    Logs.debug (fun l -> l "inotify: %s" path);
    fn path;
    iter i
  in
  Lwt_inotify.create () >>= fun i ->
  Lwt_inotify.add_watch i dir
    [Inotify.S_Create; Inotify.S_Delete; Inotify.S_Modify]
  >|= fun u ->
  let stop_iter = Irmin_watcher_core.stoppable (fun () -> iter i) in
  let stop_scheduler () = Lwt_inotify.rm_watch i u in
  fun () ->
    stop_iter ();
    stop_scheduler ()

let t = Irmin_watcher_core.Watchdog.empty ()

(* Note: we use Inotify to detect any change, and we re-read the full
   tree on every change (so very similar to active polling, but
   blocking on incoming Inotify events instead of sleeping). We could
   probably do better, but at the moment it is more robust to do so,
   to avoid possible duplicated events. *)
let hook =
  Logs.info (fun l -> l "Inotify mode");
  let open Irmin_watcher_core in
  let wait_for_changes dir () =
    let t, u = Lwt.task () in
    listen dir (fun _path -> Lwt.wakeup u (); Lwt.return_unit) >>= fun u ->
    t >>= fun () ->
    u ()
  in
  let listen dir f =
    Irmin_watcher_polling.listen ~wait_for_changes:(wait_for_changes dir) ~dir f
  in
  create t listen

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
