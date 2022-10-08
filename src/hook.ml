(*---------------------------------------------------------------------------
   Copyright (c) 2016 Thomas Gazagnaire. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Eio
open Astring
module Digests = Core.Digests

type _ Effect.t += Top_switch : Eio.Switch.t Effect.t

let top_switch () = Effect.perform Top_switch

let ( / ) = Filename.concat

let src = Logs.Src.create "irw-hook" ~doc:"Irmin watcher shared code"

module Log = (val Logs.src_log src : Logs.LOG)

(* TODO: Convert to Eio functions? *)
let list_files kind dir =
  if Sys.file_exists dir && Sys.is_directory dir then
    let d = Sys.readdir dir in
    let d = Array.to_list d in
    let d = List.map (Filename.concat dir) d in
    let d = List.filter kind d in
    let d = List.sort String.compare d in
    d
  else []

let directories dir =
  list_files (fun f -> try Sys.is_directory f with Sys_error _ -> false) dir

let files dir =
  list_files
    (fun f -> try not (Sys.is_directory f) with Sys_error _ -> false)
    dir

let rec_files dir =
  let rec aux accu dir =
    let ds = directories dir in
    let fs = files dir in
    List.fold_left aux (fs @ accu) ds
  in
  aux [] dir

let read_file ~prefix f =
  try
    if (not (Sys.file_exists f)) || Sys.is_directory f then None
    else
      let r = String.with_range ~first:(String.length prefix) f in
      Some (r, Digest.file f)
  with ex ->
    Log.info (fun fm -> fm "read_file(%s): %a" f Fmt.exn ex);
    None

let read_files dir =
  let new_files = rec_files dir in
  let prefix = dir / "" in
  List.fold_left
    (fun acc f ->
      match read_file ~prefix f with None -> acc | Some d -> Digests.add d acc)
    Digests.empty new_files

type event = [ `Unknown | `File of string ]

let rec poll n ~callback ~wait_for_changes dir files (event : event) =
  let new_files =
    match event with
      | `Unknown -> read_files dir
      | `File f -> (
      let prefix = dir / "" in
      let short_f = String.with_range ~first:(String.length prefix) f in
      let files = Digests.filter (fun (x, _) -> x <> short_f) files in
      match read_file ~prefix f with
      | None -> files
      | Some d -> Digests.add d files)
  in
  Log.debug (fun l ->
      l "files=%a new_files=%a" Digests.pp files Digests.pp new_files);
  let diff = Digests.sdiff files new_files in
  let process () =
    if Digests.is_empty diff then ()
    else (
      Log.debug (fun f -> f "[%d] polling %s: diff:%a" n dir Digests.pp diff);
      let files = Digests.files diff in
      Fiber.iter callback files)
  in
  process ();
  let event = wait_for_changes () in
  poll n ~callback ~wait_for_changes dir new_files event

let id = ref 0

let v ~sw ~wait_for_changes ~dir callback =
  let n = !id in
  incr id;
  let files = read_files dir in
  Core.stoppable ~sw (fun () ->
      poll n ~callback ~wait_for_changes dir files `Unknown)

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
