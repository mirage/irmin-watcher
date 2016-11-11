(*---------------------------------------------------------------------------
   Copyright (c) 2016 Thomas Gazagnaire. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Lwt.Infix

let src = Logs.Src.create "irw-inotify" ~doc:"Irmin watcher using Inotify"
module Log = (val Logs.src_log src : Logs.LOG)

let start_watch dir =
  Log.debug (fun l -> l "start_watch %s" dir);
  Lwt_inotify.create () >>= fun i ->
  Lwt_inotify.add_watch i dir
    [Inotify.S_Create; Inotify.S_Delete; Inotify.S_Modify]
  >|= fun u ->
  let stop () = Lwt_inotify.rm_watch i u in
  i, stop

let listen i fn =
  let path_of_event (_, _, _, p) = match p with None -> "" | Some p -> p in
  let rec iter i =
    Lwt_inotify.read i >>= fun e ->
    let path = path_of_event e in
    Log.debug (fun l -> l "inotify: %s" path);
    fn path;
    iter i
  in
  Irmin_watcher_core.stoppable (fun () -> iter i)

let t = Irmin_watcher_core.Watchdog.empty ()

(* Note: we use Inotify to detect any change, and we re-read the full
   tree on every change (so very similar to active polling, but
   blocking on incoming Inotify events instead of sleeping). We could
   probably do better, but at the moment it is more robust to do so,
   to avoid possible duplicated events. *)
let hook =
  let open Irmin_watcher_core in
  let listen dir f =
    Log.info (fun l -> l "Inotify mode");
    let events = ref [] in
    let cond = Lwt_condition.create () in
    start_watch dir >>= fun (i, stop_watch) ->
    let rec wait_for_changes () =
      match List.rev !events with
      | []   -> Lwt_condition.wait cond >>= wait_for_changes
      | h::t -> events := List.rev t; Lwt.return (`File h)
    in
    let unlisten = listen i (fun path ->
        events := path :: !events;
        Lwt_condition.broadcast cond ();
        Lwt.return_unit
      ) in
    Irmin_watcher_polling.listen ~wait_for_changes ~dir f >|= fun unpoll ->
    fun () ->
      stop_watch () >>= fun () ->
      unlisten () >>= fun () ->
      unpoll ()
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
