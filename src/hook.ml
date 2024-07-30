(*---------------------------------------------------------------------------
   Copyright (c) 2016 Thomas Gazagnaire. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Eio
module Digests = Core.Digests

let ( / ) = Eio.Path.( / )
let src = Logs.Src.create "irw-hook" ~doc:"Irmin watcher shared code"

module Log = (val Logs.src_log src : Logs.LOG)

let list_files kind dir =
  if Eio.Path.is_directory dir then
    let d = Eio.Path.read_dir dir in
    let d = List.map (fun p -> dir / p) d in
    let d = List.filter kind d in
    d
  else []

let directories dir = list_files Eio.Path.is_directory dir
let files dir = list_files Eio.Path.is_file dir

let rec_files dir =
  let rec aux accu dir =
    let ds = directories dir in
    let fs = files dir in
    List.fold_left aux (fs @ accu) ds
  in
  aux [] dir

let read_file f =
  try
    if not (Eio.Path.is_file f) then None
    else
      let file = Eio.Path.native_exn f in
      Some (Digest.file file)
  with ex ->
    Log.info (fun fm -> fm "read_file(%a): %a" Eio.Path.pp f Fmt.exn ex);
    None

let read_files dir =
  let new_files = rec_files dir in
  List.fold_left
    (fun acc path ->
      match read_file path with
      | None -> acc
      | Some d ->
          let nat_dir = Eio.Path.native_exn path in
          Digests.add (nat_dir, d) acc)
    Digests.empty new_files

type event = [ `Unknown | `File of Eio.Fs.dir_ty Eio.Path.t ]

let rec poll n ~callback ~wait_for_changes dir files (event : event) =
  let new_files =
    match event with
    | `Unknown -> read_files dir
    | `File path -> (
        let nat_dir = Eio.Path.native_exn path in
        let files = Digests.filter (fun (x, _) -> x <> nat_dir) files in
        match read_file path with
        | None -> files
        | Some d -> Digests.add (nat_dir, d) files)
  in
  Log.debug (fun l ->
      l "files=%a new_files=%a" Digests.pp files Digests.pp new_files);
  let diff = Digests.sdiff files new_files in
  let process () =
    if Digests.is_empty diff then ()
    else (
      Log.debug (fun f ->
          f "[%d] polling %a: diff:%a" n Eio.Path.pp dir Digests.pp diff);
      let files = Digests.files diff in
      Fiber.List.iter (fun file -> callback (dir / file)) files)
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
