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

val v : ?polling_time:float -> sw:Eio.Switch.t -> unit -> t
(** [v ~sw ()] is a new watcher using the {!mode} backend. With the polling
    backend, it scans the watched directories every [polling_time] seconds
    (default [1.]); [v] raises [Invalid_argument] if [polling_time] is not
    positive. The fibers watching directories are attached to [sw]. A program
    usually creates a single watcher and registers all its hooks on it.

    Once [sw] is cancelled, the watcher stops watching, and {!hook} raises
    [Invalid_argument]. [v] raises [Invalid_argument] if [sw] is finished. An
    error of the watcher itself, such as a failure to read Inotify events, fails
    [sw].

    With the FSevents backend, the watcher runs Lwt code through [Lwt_eio]: an
    [Lwt_eio] event loop must be running (see [Lwt_eio.with_event_loop]). *)

val hook :
  t -> int -> Eio.Fs.dir_ty Eio.Path.t -> (string -> unit) -> unit -> unit
(** [hook t id p f] calls [f] everytime a file under [p] is created, modified or
    removed, and returns a function that removes the hook. [f] receives the path
    of the file relative to [p], using ["/"] as separator. [id] is a unique
    identifier distinguishing the hooks registered on the same path.

    [hook] returns once [p] has been read: later changes are reported. It raises
    the errors met while starting to watch [p]. The files and directories under
    [p] that cannot be read are logged, keep their previous state, and are read
    again later. With the Inotify backend, [p] is created if it does not exist,
    only the changes to the files directly in [p] are reported, and no change is
    reported any more once [p] is removed.

    [f] runs in a fiber attached to the switch of [t], possibly at the same time
    as other callbacks. The watcher waits for the callbacks before looking for
    the next changes in [p]: a blocking [f] delays the other hooks on [p]. The
    exceptions it raises are logged. [f] is not called once the hook is removed,
    and removing the last hook on [p] from [f] cancels [f]. *)

type stats = { watchdogs : int; dispatches : int }
(** The type for watcher statistics: the number of active watchdogs, and the
    number of registered hooks, for all directories. *)

val stats : t -> stats
(** [stats t] is a snapshot of [t]'s statistics. *)

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
