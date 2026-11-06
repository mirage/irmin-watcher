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

(* Files are named by their path relative to the watched directory, using
   ["/"] as separator. *)
let concat rel name = if rel = "" then name else rel ^ "/" ^ name
let path dir rel = if rel = "" then dir else dir / rel

let list_files kind dir rel =
  let p = path dir rel in
  if Eio.Path.is_directory p then
    Eio.Path.read_dir p
    |> List.map (concat rel)
    |> List.filter (fun rel -> kind (dir / rel))
  else []

let directories = list_files Eio.Path.is_directory
let files = list_files Eio.Path.is_file

let rec_files dir =
  let rec aux accu rel =
    let ds = directories dir rel in
    let fs = files dir rel in
    List.fold_left aux (fs @ accu) ds
  in
  aux [] ""

let read_file dir rel =
  let f = dir / rel in
  try
    if not (Eio.Path.is_file f) then None
    else
      let file = Eio.Path.native_exn f in
      Some (Digest.file file)
  with ex ->
    Log.info (fun fm -> fm "read_file(%a): %a" Eio.Path.pp f Fmt.exn ex);
    None

let read_files dir =
  List.fold_left
    (fun acc rel ->
      match read_file dir rel with
      | None -> acc
      | Some d -> Digests.add (rel, d) acc)
    Digests.empty (rec_files dir)

type event = [ `Unknown | `File of string ]

let rec poll n ~callback ~wait_for_changes dir files (event : event) =
  let new_files =
    match event with
    | `Unknown -> read_files dir
    | `File rel -> (
        let files = Digests.filter (fun (x, _) -> x <> rel) files in
        match read_file dir rel with
        | None -> files
        | Some d -> Digests.add (rel, d) files)
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
      Fiber.List.iter callback files)
  in
  process ();
  let event = wait_for_changes () in
  poll n ~callback ~wait_for_changes dir new_files event

let id = ref 0

let run ~ready ~wait_for_changes ~dir callback =
  let n = !id in
  incr id;
  let files = read_files dir in
  ready ();
  poll n ~callback ~wait_for_changes dir files `Unknown

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
