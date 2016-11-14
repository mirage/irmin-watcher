(*---------------------------------------------------------------------------
   Copyright (c) 2016 Thomas Gazagnaire. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Active polling backend for Irmin watchers.

    {e %%VERSION%% — {{:%%PKG_HOMEPAGE%% }homepage}} *)

open Irmin_watcher_core

val hook: float -> t Lwt.t
(** [hook delay id p f] is the hook calling [f] everytime a sub-path
    of [p] is modified. Return a function to call to remove the
    hook. Active polling is done every [delay] seconds. *)

type event = [ `Unknown | `File of string ]
(** The type for change event. *)

val listen: wait_for_changes:(unit -> event Lwt.t) -> dir:string ->
  Watchdog.hook
(** [listen ~wait_for_changes ~dir] is the watchdog hook using
    [wait_for_changes] to detect filesystem updates in the directory
    [dir]. The polling implemention just calls [Lwt_unix.sleep]. *)

val default_polling_time: float ref
(** The default interval for active polling, in seconds. By default
    it is 1s. *)

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
