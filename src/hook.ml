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

let is_missing = function
  | Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> true
  | _ -> false

(* [digest f] is the digest of the regular file [f], or [None] if [f] has been
   removed. It raises the other errors. [Digest.file] reads [f] in constant
   memory. *)
let digest f =
  try Some (Digest.file (Eio.Path.native_exn f))
  with Sys_error _ when Eio.Path.kind ~follow:true f = `Not_found -> None

(* [read_file dir rel] is the digest of the file [rel], or [None] if it is not
   a regular file. It raises the errors other than a missing file. *)
let read_file dir rel =
  let f = dir / rel in
  match Eio.Path.kind ~follow:true f with
  | `Regular_file -> digest f
  | _ -> None

(* The result of a scan: the digests of the files that could be read, and the
   paths of the files and directories that could not. *)
type scan = { digests : Digests.t; failed : string list }

let scan dir =
  let failed acc rel ex =
    Log.warn (fun l ->
        l "cannot read %a: %a" Eio.Path.pp (path dir rel) Fmt.exn ex);
    { acc with failed = rel :: acc.failed }
  in
  let rec read_dir acc rel =
    match Eio.Path.read_dir (path dir rel) with
    | names ->
        List.fold_left
          (fun acc name -> read_entry acc (concat rel name))
          acc names
    | exception (Eio.Io _ as ex) when is_missing ex ->
        (* The directory can be removed while it is being read. *)
        acc
    | exception (Eio.Io _ as ex) -> failed acc rel ex
  and read_entry acc rel =
    let f = dir / rel in
    match Eio.Path.kind ~follow:true f with
    | `Directory -> read_dir acc rel
    | `Regular_file -> (
        match digest f with
        | Some d -> { acc with digests = Digests.add (rel, d) acc.digests }
        | None -> acc
        | exception ((Eio.Io _ | Sys_error _) as ex) -> failed acc rel ex)
    | _ -> acc
    | exception (Eio.Io _ as ex) -> failed acc rel ex
  in
  let empty = { digests = Digests.empty; failed = [] } in
  match Eio.Path.kind ~follow:true dir with
  | `Directory -> read_dir empty ""
  | _ -> empty
  | exception (Eio.Io _ as ex) -> failed empty "" ex

(* [merge ~previous scan] is the digests of [scan], plus the digests of
   [previous] for the paths that could not be read: their changes are not
   known yet. *)
let merge ~previous { digests; failed } =
  let under rel failed =
    failed = "" || rel = failed || String.starts_with ~prefix:(failed ^ "/") rel
  in
  if failed = [] then digests
  else
    Digests.union digests
      (Digests.filter (fun (rel, _) -> List.exists (under rel) failed) previous)

type event = [ `Unknown | `File of string ]

(* [update dir files event] is the new state of [dir] after [event], and
   whether all the files concerned could be read. *)
let update dir files (event : event) =
  match event with
  | `Unknown ->
      let scan = scan dir in
      (merge ~previous:files scan, scan.failed = [])
  | `File rel -> (
      let others = Digests.filter (fun (x, _) -> x <> rel) files in
      match read_file dir rel with
      | None -> (others, true)
      | Some d -> (Digests.add (rel, d) others, true)
      | exception ((Eio.Io _ | Sys_error _) as ex) ->
          Log.warn (fun l ->
              l "cannot read %a: %a" Eio.Path.pp (dir / rel) Fmt.exn ex);
          (files, false))

let report n ~callback dir files new_files =
  Log.debug (fun l ->
      l "files=%a new_files=%a" Digests.pp files Digests.pp new_files);
  let diff = Digests.sdiff files new_files in
  if not (Digests.is_empty diff) then (
    Log.debug (fun f ->
        f "[%d] polling %a: diff:%a" n Eio.Path.pp dir Digests.pp diff);
    Fiber.List.iter callback (Digests.files diff))

(* After a failed read, rescan after [delay] seconds even without a new event,
   doubling the delay up to a minute while reads keep failing. *)
let next_delay = function
  | None -> 1.
  | Some delay -> Float.min 60. (2. *. delay)

let id = ref 0

let run ~ready ~wait_for_changes ~dir callback =
  let n = !id in
  incr id;
  let wait = function
    | None -> wait_for_changes ()
    | Some delay ->
        Fiber.first wait_for_changes (fun () ->
            Eio_unix.sleep delay;
            `Unknown)
  in
  let rec loop files ~retry =
    let new_files, complete = update dir files (wait retry) in
    report n ~callback dir files new_files;
    loop new_files ~retry:(if complete then None else Some (next_delay retry))
  in
  let scan = scan dir in
  ready ();
  loop scan.digests
    ~retry:(if scan.failed = [] then None else Some (next_delay None))

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
