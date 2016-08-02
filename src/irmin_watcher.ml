(*---------------------------------------------------------------------------
   Copyright (c) 2016 Thomas Gazagnaire. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Lwt.Infix
open Astring

let (/) = Filename.concat

let src = Logs.Src.create "irmin-watcher" ~doc:"Irmin watcher logging"
module Log = (val Logs.src_log src : Logs.LOG)

let default_polling_cycle = ref 1.

type hook = int -> string -> (string -> unit Lwt.t) -> (unit -> unit) Lwt.t

(* run [t] and returns an handler to stop the task. *)
let stoppable t =
  let s, u = Lwt.task () in
  Lwt.async (fun () -> Lwt.pick ([s; t ()]));
  function () -> Lwt.wakeup u ()

(* Active polling *)
module Polling = struct

  module S = struct
    include Set.Make(struct
        type t = string * Digest.t
        let compare = compare
      end)
    let of_list l = List.fold_left (fun set elt -> add elt set) empty l
    let sdiff x y = union (diff x y) (diff y x)
    let digest_pp ppf d = Fmt.string ppf @@ Digest.to_hex d
    let pp ppf t = Fmt.(Dump.list (pair string digest_pp)) ppf @@ elements t
  end

  let list_files kind dir =
    if Sys.file_exists dir && Sys.is_directory dir then
      let d = Sys.readdir dir in
      let d = Array.to_list d in
      let d = List.map (Filename.concat dir) d in
      let d = List.filter kind d in
      let d = List.sort String.compare d in
      Lwt.return d
    else
    Lwt.return_nil

  let directories dir =
    list_files (fun f ->
        try Sys.is_directory f with Sys_error _ -> false
      ) dir

  let files dir =
    list_files (fun f ->
        try not (Sys.is_directory f) with Sys_error _ -> false
      ) dir

  let rec_files dir =
    let rec aux accu dir =
      directories dir >>= fun ds ->
      files dir       >>= fun fs ->
      Lwt_list.fold_left_s aux (fs @ accu) ds in
    aux [] dir

  let read_files dir =
    rec_files dir >>= fun new_files ->
    let prefix = dir / "" in
    let new_files =
      List.map (fun f ->
          String.with_range ~first:(String.length prefix) f, Digest.file f
        ) new_files
    in
    Lwt.return (S.of_list new_files)

  (* active polling *)
  let rec poll ~callback ~delay dir files =
    read_files dir >>= fun new_files ->
    let diff = S.sdiff files new_files in
    begin if S.is_empty diff then (
        Log.debug (fun f -> f "polling %s: no changes!" dir);
        Lwt.return_unit
      ) else (
        Log.debug (fun f -> f "polling %s: diff:%a" dir S.pp diff);
        let files =
          S.elements diff
          |> List.map fst
          |> String.Set.of_list
          |> String.Set.elements
        in
        Lwt_list.iter_p (callback dir) files)
    end >>= fun () ->
    Lwt_unix.sleep delay >>= fun () ->
    poll ~callback ~delay dir new_files

  let listen ~callback ~delay dir =
    read_files dir >|= fun files ->
    stoppable (fun () -> poll ~callback ~delay dir files)

  (* map directory names to list of callbacks *)
  let listeners = Hashtbl.create 10
  let watchdogs = Hashtbl.create 10

  let nb_listeners dir =
    try List.length (Hashtbl.find listeners dir) with Not_found -> 0

  let watchdog dir =
    try Some (Hashtbl.find watchdogs dir) with Not_found -> None

  (* call all the callbacks on the file *)
  let callback dir file =
    let fns = try Hashtbl.find listeners dir with Not_found -> [] in
    Lwt_list.iter_p (fun (id, f) ->
        Log.debug (fun f -> f "callback %d" id); f file
      ) fns

  let realdir dir =
    if Filename.is_relative dir then Sys.getcwd () / dir else dir

  let start_watchdog ~delay dir =
    match watchdog dir with
    | Some _ -> assert (nb_listeners dir <> 0); Lwt.return_unit
    | None   ->
        (* Note: multiple threads can wait here *)
        listen dir ~delay ~callback >|= fun u ->
        match watchdog dir with
        | Some _ -> u ()
        | None   ->
            Log.debug (fun f -> f "Start watchdog for %s" dir);
            Hashtbl.add watchdogs dir u

  let stop_watchdog dir =
    match watchdog dir with
    | None      -> assert (nb_listeners dir = 0)
    | Some stop ->
        if nb_listeners dir = 0 then (
          Log.debug (fun f -> f "Stop watchdog for %s" dir);
          Hashtbl.remove watchdogs dir;
          stop ()
        )

  let add_listener id dir fn =
    let fns = try Hashtbl.find listeners dir with Not_found -> [] in
    let fns = (id, fn) :: fns in
    Hashtbl.replace listeners dir fns

  let remove_listener id dir =
    let fns = try Hashtbl.find listeners dir with Not_found -> [] in
    let fns = List.filter (fun (x,_) -> x <> id) fns in
    if fns = [] then Hashtbl.remove listeners dir
    else Hashtbl.replace listeners dir fns

  let uninstall_dir_polling_listener () =
    Hashtbl.iter (fun _dir stop -> stop ()) watchdogs;
    Hashtbl.clear watchdogs;
    Hashtbl.clear listeners

  let hook delay: hook =
    uninstall_dir_polling_listener ();
    let listen_dir id dir fn =
      let dir = realdir dir in
      start_watchdog ~delay dir >|= fun () ->
      add_listener id dir fn;
      function () ->
        remove_listener id dir;
        stop_watchdog dir
    in
    listen_dir

end

let hook id dir fn =
#ifdef HAVE_FSEVENTS
  let _ = Polling.hook in
  Irmin_watcher_fsevents.hook id dir fn
#else
  Polling.hook !default_polling_cycle id dir fn
#endif

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
