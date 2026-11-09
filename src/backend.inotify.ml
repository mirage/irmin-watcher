(*---------------------------------------------------------------------------
   Copyright (c) 2016 Thomas Gazagnaire. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

let src = Logs.Src.create "irw-inotify" ~doc:"Irmin watcher using Inotify"

module Log = (val Logs.src_log src : Logs.LOG)

(* [start_watch ~sw dir] is an inotify descriptor watching [dir]. It is closed
   when [sw] is released, which also removes the watch. *)
let start_watch ~sw dir =
  Log.debug (fun l -> l "start_watch %a" Eio.Path.pp dir);
  (* [dir] may be created concurrently by another hook. *)
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 dir;
  let i = Eio_inotify.create () in
  Eio.Switch.on_release sw (fun () -> Eio_inotify.close i);
  let _ : Inotify.watch =
    Eio_inotify.add_watch i (Eio.Path.native_exn dir)
      [ Inotify.S_Create; Inotify.S_Modify; Inotify.S_Move; Inotify.S_Delete ]
  in
  i

let listen dir i fn =
  let pp_kind = Fmt.of_to_string Inotify.string_of_event_kind in
  let rec iter () =
    let _, es, _, name = Eio_inotify.read i in
    Log.debug (fun l ->
        l "inotify: %a %a %a" Eio.Path.pp dir
          Fmt.(option string)
          name
          Fmt.(Dump.list pp_kind)
          es);
    (* An event without a file name is about [dir] itself, or a queue
       overflow: rescan the whole directory. *)
    fn (match name with None -> `Unknown | Some name -> `File name);
    iter ()
  in
  iter ()

(* Note: we use Inotify to detect any change, and we re-read the full
   tree on every change (so very similar to active polling, but
   blocking on incoming Inotify events instead of sleeping). We could
   probably do better, but at the moment it is more robust to do so,
   to avoid possible duplicated events. *)
let v ~sw ~polling_time:_ =
  let listen dir f =
    Log.info (fun l -> l "Inotify mode");
    Core.spawn ~sw (fun ~sw ~ready ->
        let i = start_watch ~sw dir in
        let events = Eio.Stream.create max_int in
        let wait_for_changes () = Eio.Stream.take events in
        Eio.Fiber.both
          (fun () -> listen dir i (Eio.Stream.add events))
          (fun () -> Hook.run ~ready ~wait_for_changes ~dir f))
  in
  Core.create ~sw listen

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
