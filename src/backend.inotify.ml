(*---------------------------------------------------------------------------
   Copyright (c) 2016 Thomas Gazagnaire. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

let src = Logs.Src.create "irw-inotify" ~doc:"Irmin watcher using Inotify"

module Log = (val Logs.src_log src : Logs.LOG)

let mkdir d =
  let perm = 0o0700 in
  Eio.Path.mkdirs ~perm d

let start_watch dir =
  Log.debug (fun l -> l "start_watch %a" Eio.Path.pp dir);
  if Eio.Path.kind ~follow:false dir = `Not_found then mkdir dir;
  let i = Eio_inotify.create () in
  let u =
    Eio_inotify.add_watch i (Eio.Path.native_exn dir)
      [ Inotify.S_Create; Inotify.S_Modify; Inotify.S_Move; Inotify.S_Delete ]
  in
  let stop () =
    Eio_inotify.rm_watch i u;
    Eio_inotify.close i
  in
  (i, stop)

let listen ~sw dir i fn =
  let event_kinds (_, es, _, _) = es in
  let pp_kind = Fmt.of_to_string Inotify.string_of_event_kind in
  let path_of_event (_, _, _, p) =
    match p with None -> dir | Some p -> Eio.Path.(dir / p)
  in
  let rec iter i =
    let e = Eio_inotify.read i in
    let path = path_of_event e in
    let es = event_kinds e in
    Log.debug (fun l ->
        l "inotify: %a %a" Eio.Path.pp path Fmt.(Dump.list pp_kind) es);
    fn path;
    iter i
  in
  Core.stoppable ~sw (fun () -> iter i)

(* Note: we use Inotify to detect any change, and we re-read the full
   tree on every change (so very similar to active polling, but
   blocking on incoming Inotify events instead of sleeping). We could
   probably do better, but at the moment it is more robust to do so,
   to avoid possible duplicated events. *)
let v ~sw =
  let open Eio in
  let listen dir f =
    Log.info (fun l -> l "Inotify mode");
    let events = ref [] in
    let cond = Condition.create () in
    let i, stop_watch = start_watch dir in
    let rec wait_for_changes () =
      match List.rev !events with
      | [] ->
          Condition.await_no_mutex cond;
          wait_for_changes ()
      | h :: t ->
          events := List.rev t;
          `File h
    in
    let unlisten =
      listen ~sw dir i (fun path ->
          events := path :: !events;
          Condition.broadcast cond)
    in
    Hook.v ~sw ~wait_for_changes ~dir f |> fun unpoll () ->
    stop_watch ();
    unlisten ();
    unpoll ()
  in
  Core.create listen

let mode = `Inotify

let uname () =
  try
    let ic = Unix.open_process_in "uname" in
    let uname = input_line ic in
    let () = close_in ic in
    Some uname
  with Unix.Unix_error _ -> None

let is_linux () = Sys.os_type = "Unix" && uname () = Some "Linux"

type mode = [ `Polling | `Inotify ]

let mode, v =
  if is_linux () then ((mode :> mode), v) else Polling.((mode :> mode), v)

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
