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

(* The digests of the files read before, with their status at that time. A
   scan does not read a file again if its status has not changed since, unless
   the file had changed shortly before it was read: file times have a limited
   precision, so a later change could have kept the same status. This assumes
   that the clock of the file system agrees with the system clock, and that
   any change of a file updates its status change time (ctime). On Windows,
   ctime is the creation time: a copy that keeps the modification time could
   change a file without changing its status, so the cache is not used. *)
module Cache = struct
  module Map = Map.Make (String)

  type entry = { stat : Eio.File.Stat.t; digest : Digest.t; time : float }
  type t = entry Map.t

  (* Changes less than [racy] seconds apart can leave the same file times. *)
  let racy = 2.

  let same (a : Eio.File.Stat.t) (b : Eio.File.Stat.t) =
    Int64.equal a.dev b.dev
    && Int64.equal a.ino b.ino
    && Optint.Int63.equal a.size b.size
    && Float.equal a.mtime b.mtime
    && Float.equal a.ctime b.ctime

  (* [valid e] is true if the file of [e] had not changed shortly before it
     was read. *)
  let valid e = Float.max e.stat.mtime e.stat.ctime < e.time -. racy

  (* [find t rel stat] is the entry of [rel] in [t], if it is valid and [rel]
     still has the status [stat]. *)
  let find t rel stat =
    match Map.find_opt rel t with
    | Some e when (not Sys.win32) && valid e && same e.stat stat -> Some e
    | _ -> None
end

(* [read f stat] is the cache entry of the regular file [f] of status [stat],
   or [None] if [f] has been removed. It raises the other errors. *)
let read f (stat : Eio.File.Stat.t) =
  (* The system time, to be compared with the times of [f]. *)
  let time = Unix.gettimeofday () in
  Option.map
    (fun digest -> { Cache.stat; digest; time })
    (digest ~size:stat.size f)

(* [read_cached ~cache f rel stat] is like [read f stat], but it is the entry
   of [rel] in [cache] when that entry is still valid. *)
let read_cached ~cache f rel stat =
  match Cache.find cache rel stat with Some _ as e -> e | None -> read f stat

(* [read_file dir rel] is the cache entry of the file [rel], or [None] if a
   scan would skip it or if it is a directory. It raises the errors other than
   a missing file. *)
let read_file dir rel =
  let f = dir / rel in
  match entry f with `File stat -> read f stat | `Directory | `Skip -> None

module Paths = Set.Make (String)

(* [log_failure ~known dir rel ex] logs that [rel] cannot be read. A path in
   [known] already failed before: it is only logged at the debug level, so
   that a path that stays unreadable does not log a warning at every scan. *)
let log_failure ~known dir rel ex =
  let level = if Paths.mem rel known then Logs.Debug else Logs.Warning in
  Log.msg level (fun l ->
      l "cannot read %a: %a" Eio.Path.pp (path dir rel) Fmt.exn ex)

(* The result of a scan: the digests of the files that could be read, the
   paths of the files and directories that could not, and the cache entries of
   the files read. *)
type scan = { digests : Digests.t; failed : string list; cache : Cache.t }

(* [scan ~cache ~known dir] reads the files of [dir], except those whose entry
   in [cache] is still valid. *)
let scan ~cache ~known dir =
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
    | `File stat -> (
        match read_cached ~cache f rel stat with
        | Some e ->
            {
              acc with
              digests = Digests.add (rel, e.digest) acc.digests;
              cache = Cache.Map.add rel e acc.cache;
            }
        | None -> acc
        | exception ((Eio.Io _ | Sys_error _) as ex) -> failed acc rel ex)
    | `Skip -> acc
    | exception (Eio.Io _ as ex) -> failed acc rel ex
  in
  let empty =
    { digests = Digests.empty; failed = []; cache = Cache.Map.empty }
  in
  match Eio.Path.kind ~follow:true dir with
  | `Directory -> read_dir empty ""
  | _ -> empty
  | exception (Eio.Io _ as ex) -> failed empty "" ex

(* [merge ~previous scan] is the digests of [scan], plus the digests of
   [previous] for the paths that could not be read: their changes are not
   known yet. *)
let merge ~previous { digests; failed; _ } =
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

(* The state of a watched directory: the digests of its files, their cache
   entries, and the paths that are known to fail. *)
type state = { files : Digests.t; cache : Cache.t; known : Paths.t }

let state_of_scan scan =
  {
    files = scan.digests;
    cache = scan.cache;
    known = Paths.of_list scan.failed;
  }

(* [update dir state event] is the new state of [dir] after [event], and
   whether all the files concerned could be read. *)
let update dir { files; cache; known } (event : event) =
  match event with
  | `Unknown ->
      let scan = scan ~cache ~known dir in
      ( { (state_of_scan scan) with files = merge ~previous:files scan },
        scan.failed = [] )
  | `Files rels ->
      List.fold_left
        (fun (({ files; cache; known } as state), complete) rel ->
          match read_file dir rel with
          | entry ->
              let files = remove_file rel files in
              let files, cache =
                match entry with
                | None -> (files, Cache.Map.remove rel cache)
                | Some e ->
                    ( Digests.add (rel, e.digest) files,
                      Cache.Map.add rel e cache )
              in
              ({ files; cache; known = Paths.remove rel known }, complete)
          | exception ((Eio.Io _ | Sys_error _) as ex) ->
              (* [rel] keeps its previous state. *)
              log_failure ~known dir rel ex;
              ({ state with known = Paths.add rel known }, false))
        ({ files; cache; known }, true)
        rels

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
  let rec loop state ~retry =
    let event = wait retry in
    let new_state, complete = update dir state event in
    report n ~callback dir state.files new_state.files;
    let retry =
      match (event, complete, retry) with
      | `Unknown, true, _ -> None
      | `Unknown, false, _ | `Files _, false, None -> next_retry retry
      | `Files _, _, _ -> retry
    in
    loop new_state ~retry
  in
  if not (Eio.Path.is_directory dir) then
    Log.info (fun l ->
        l "%a is not a directory: nothing is reported until it is one"
          Eio.Path.pp dir);
  let scan = scan ~cache:Cache.Map.empty ~known:Paths.empty dir in
  ready ();
  loop (state_of_scan scan)
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
