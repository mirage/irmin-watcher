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

let is_symlink_loop = function
  | Eio.Io (Eio.Fs.E Eio.Fs.Symlink, _) -> true
  | _ -> false

(* Files larger than this are digested in a system thread, so that reading them
   does not block the other fibers. Smaller files are read directly, which is
   cheaper. *)
let large_file = 1 lsl 20

(* [digest ~size f] is the digest of the regular file [f] of [size] bytes, or
   [None] if [f] has been removed. It raises the other errors. [Digest.file]
   reads [f] in constant memory. *)
let digest ~size f =
  let path = Eio.Path.native_exn f in
  let digest () = Digest.file path in
  try
    Some
      (if Optint.Int63.to_int size > large_file then
         Eio_unix.run_in_systhread ~label:"irmin-watcher digest" digest
       else digest ())
  with Sys_error _ when Eio.Path.kind ~follow:true f = `Not_found -> None

(* [entry f] is what a scan does with [f]: [`Directory] if it is a directory,
   [`File stat] if it is a regular file or a symbolic link to one, of status
   [stat], and [`Skip] otherwise, including when [f] is missing. Symbolic links
   to directories are not followed: they can lead out of the watched
   directory, or back into it. It raises the other errors. *)
let entry f =
  match Eio.Path.stat ~follow:false f with
  | { kind = `Directory; _ } -> `Directory
  | { kind = `Regular_file; _ } as stat -> `File stat
  | { kind = `Symbolic_link; _ } -> (
      match Eio.Path.stat ~follow:true f with
      | { kind = `Regular_file; _ } as stat -> `File stat
      | _ -> `Skip
      | exception (Eio.Io _ as ex) when is_missing ex || is_symlink_loop ex ->
          (* A dangling symbolic link, or a loop of symbolic links. *)
          `Skip)
  | _ -> `Skip
  | exception (Eio.Io _ as ex) when is_missing ex -> `Skip

(* [read_file dir rel] is the digest of the file [rel], or [None] if a scan
   would skip it or if it is a directory. It raises the errors other than a
   missing file. *)
let read_file dir rel =
  let f = dir / rel in
  match entry f with
  | `File { size; _ } -> digest ~size f
  | `Directory | `Skip -> None

module Paths = Set.Make (String)

(* [log_failure ~known dir rel ex] logs that [rel] cannot be read. A path in
   [known] already failed before: it is only logged at the debug level, so
   that a path that stays unreadable does not log a warning at every scan. *)
let log_failure ~known dir rel ex =
  let level = if Paths.mem rel known then Logs.Debug else Logs.Warning in
  Log.msg level (fun l ->
      l "cannot read %a: %a" Eio.Path.pp (path dir rel) Fmt.exn ex)

(* The result of a scan: the digests of the files that could be read, and the
   paths of the files and directories that could not. *)
type scan = { digests : Digests.t; failed : string list }

let scan ~known dir =
  let failed acc rel ex =
    log_failure ~known dir rel ex;
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
    let f = path dir rel in
    match entry f with
    | `Directory -> read_dir acc rel
    | `File { size; _ } -> (
        match digest ~size f with
        | Some d -> { acc with digests = Digests.add (rel, d) acc.digests }
        | None -> acc
        | exception ((Eio.Io _ | Sys_error _) as ex) -> failed acc rel ex)
    | `Skip -> acc
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
  if failed = [] then digests
  else if List.mem "" failed then Digests.union digests previous
  else
    let failed = Paths.of_list failed in
    (* [rel] is under a failed path if it or one of its parents failed. *)
    let rec under rel =
      Paths.mem rel failed
      ||
      match String.rindex_opt rel '/' with
      | Some i -> under (String.sub rel 0 i)
      | None -> false
    in
    Digests.union digests (Digests.filter (fun (rel, _) -> under rel) previous)

type event = [ `Unknown | `Files of string list ]

(* [remove_file rel files] is [files] without the digest of the file [rel],
   which has at most one. *)
let remove_file rel files =
  match
    Digests.find_first_opt (fun (x, _) -> String.compare x rel >= 0) files
  with
  | Some ((x, _) as elt) when String.equal x rel -> Digests.remove elt files
  | _ -> files

(* [update ~known dir files event] is the new state of [dir] after [event],
   whether all the files concerned could be read, and the paths that are known
   to fail. *)
let update ~known dir files (event : event) =
  match event with
  | `Unknown ->
      let scan = scan ~known dir in
      (merge ~previous:files scan, scan.failed = [], Paths.of_list scan.failed)
  | `Files rels ->
      List.fold_left
        (fun (files, complete, known) rel ->
          match read_file dir rel with
          | None -> (remove_file rel files, complete, Paths.remove rel known)
          | Some d ->
              ( Digests.add (rel, d) (remove_file rel files),
                complete,
                Paths.remove rel known )
          | exception ((Eio.Io _ | Sys_error _) as ex) ->
              log_failure ~known dir rel ex;
              (files, false, Paths.add rel known))
        (files, true, known) rels

let report n ~callback dir files new_files =
  Log.debug (fun l ->
      l "files=%a new_files=%a" Digests.pp files Digests.pp new_files);
  let diff = Digests.sdiff files new_files in
  if not (Digests.is_empty diff) then (
    Log.debug (fun f ->
        f "[%d] polling %a: diff:%a" n Eio.Path.pp dir Digests.pp diff);
    Fiber.List.iter callback (Digests.files diff))

(* After a failed read, rescan once a delay has passed, even without a new
   event. The delay doubles up to a minute while full scans keep failing. Only
   a complete full scan cancels the pending rescan. *)
type retry = { deadline : float; delay : float }

(* [now ()] is the time in seconds on a monotonic clock, which a change of the
   system time does not affect. *)
let now () = Mtime.Span.to_float_ns (Mtime_clock.elapsed ()) *. 1e-9
let schedule delay = Some { deadline = now () +. delay; delay }

let next_retry = function
  | None -> schedule 1.
  | Some { delay; _ } -> schedule (Float.min 60. (2. *. delay))

let id = ref 0

let run ~ready ~wait_for_changes ~dir callback =
  let n = !id in
  incr id;
  let wait = function
    | None -> wait_for_changes ()
    | Some { deadline; _ } ->
        Fiber.first wait_for_changes (fun () ->
            Eio_unix.sleep (Float.max 0. (deadline -. now ()));
            `Unknown)
  in
  let rec loop files ~known ~retry =
    let event = wait retry in
    let new_files, complete, known = update ~known dir files event in
    report n ~callback dir files new_files;
    let retry =
      match (event, complete, retry) with
      | `Unknown, true, _ -> None
      | `Unknown, false, _ | `Files _, false, None -> next_retry retry
      | `Files _, _, _ -> retry
    in
    loop new_files ~known ~retry
  in
  if not (Eio.Path.is_directory dir) then
    Log.info (fun l ->
        l "%a is not a directory: nothing is reported until it is one"
          Eio.Path.pp dir);
  let scan = scan ~known:Paths.empty dir in
  ready ();
  loop scan.digests
    ~known:(Paths.of_list scan.failed)
    ~retry:(if scan.failed = [] then None else next_retry None)

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
