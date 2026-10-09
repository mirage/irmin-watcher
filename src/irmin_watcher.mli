(*---------------------------------------------------------------------------
   Copyright (c) 2016 Thomas Gazagnaire. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Irmin watchers.

    {e %%VERSION%% — {{:%%PKG_HOMEPAGE%%} homepage}} *)

val mode : [ `FSEvents | `Inotify | `Polling ]
(** [mode] is the backend used by the watchers: FSevents on macOS, Inotify on
    Linux and polling elsewhere. *)

type t
(** The type for watchers. A watcher runs at most one watchdog per directory,
    shared by all the hooks registered on that directory. *)

val v : sw:Eio.Switch.t -> t
(** [v ~sw] is a new watcher using the {!mode} backend. The fibers watching
    directories are attached to [sw]. A program usually creates a single watcher
    and registers all its hooks on it.

    With the FSevents backend, the watcher runs Lwt code through [Lwt_eio]: an
    [Lwt_eio] event loop must be running (see [Lwt_eio.with_event_loop]). *)

val hook :
  t ->
  int ->
  Eio.Fs.dir_ty Eio.Path.t ->
  (Eio.Fs.dir_ty Eio.Path.t -> unit) ->
  unit ->
  unit
(** [hook t id p f] is the listen hook calling [f] everytime a sub-path of [p]
    is modified. [id] should be a unique identifier used to distinguish between
    different hooks registered on the same path. Return a function to call to
    remove the hook. *)

type stats = { watchdogs : int; dispatches : int }
(** The type for watcher statistics: the number of active watchdogs, and the
    number of registered hooks. *)

val stats : t -> stats
(** [stats t] is a snapshot of [t]'s statistics. *)

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
