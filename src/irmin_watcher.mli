(*---------------------------------------------------------------------------
   Copyright (c) 2016 Thomas Gazagnaire. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Irmin watchers.

    {e %%VERSION%% — {{:%%PKG_HOMEPAGE%%} homepage}} *)

val mode : [ `FSEvents | `Inotify | `Polling ]

type stats = { watchdogs : int; dispatches : int }

val hook :
  sw:Eio.Switch.t ->
  int ->
  Eio.Fs.dir_ty Eio.Path.t ->
  (Eio.Fs.dir_ty Eio.Path.t -> unit) ->
  unit ->
  unit
(** [hook ~sw id p f] is the listen hook calling [f] everytime a sub-path of [p]
    is modified. [id] will be used to make a distinction between hooks to a same
    file and should therefore be a unique identifier. Return a function to call
    to remove the hook. Default to polling if no better solution is available.
    FSevents and Inotify backends are available. *)

val stats : sw:Eio.Switch.t -> unit -> stats
(** [stats ~sw ()] is a stats snapshot. *)

val set_polling_time : float -> unit
(** [set_polling_time f] set the polling interval to [f]. Only valid when
    [mode = `Polling]. *)

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
