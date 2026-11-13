(*---------------------------------------------------------------------------
   Copyright (c) 2016 Thomas Gazagnaire. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Directory scanning shared by the Irmin watcher backends.

    {e %%VERSION%% — {{:%%PKG_HOMEPAGE%%} homepage}} *)

type event = [ `Unknown | `Files of string list ]
(** The type for change event. [`Files fs] is a change of the files [fs], given
    by their path relative to the watched directory. [`Unknown] is a change
    anywhere in the watched directory. *)

val run :
  ready:(unit -> unit) ->
  wait_for_changes:(unit -> event) ->
  dir:Eio.Fs.dir_ty Eio.Path.t ->
  (string -> unit) ->
  unit
(** [run ~ready ~wait_for_changes ~dir f] reads the files of the directory
    [dir], calls [ready ()], then calls [f] on every file that changes, with its
    path relative to [dir]. It calls [wait_for_changes] to wait for the next
    filesystem update, which can be a list of files or [`Unknown]. [`Unknown]
    reads again only the files whose status (size, times, inode) changed, and
    the files that changed shortly before they were read. The files and
    directories that cannot be read are logged, keep their previous state, and
    are read again later, after a delay growing up to a minute. It never
    returns. *)

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
