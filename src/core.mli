(*---------------------------------------------------------------------------
   Copyright (c) 2016 Thomas Gazagnaire. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Core functions for Irmin watchers.

    {e %%VERSION%% — {{:%%PKG_HOMEPAGE%%} homepage}} *)

(** Sets of filenames and their digests. *)
module Digests : sig
  include Set.S with type elt = string * Digest.t

  val pp_elt : elt Fmt.t
  (** [pp_elt] is the pretty-printing function for digest elements. *)

  val pp : t Fmt.t
  (** [pp] is the pretty-printer for digest sets. *)

  val sdiff : t -> t -> t
  (** [sdiff x y] is [union (diff x y) (diff y x)]. *)

  val files : t -> string list
  (** [files t] is the list of files whose digest is stored in [t]. *)
end

(** Dispatch listening functions. *)
module Dispatch : sig
  type t
  (** The type for callback dispatches. *)

  val empty : unit -> t
  (** [create ()] is the empty dispatch table. *)

  val clear : t -> unit
  (** [clear t] clears the contents of the dispatch table [t]. All previous
      callbacks are discarded. *)

  val stats : t -> dir:Eio.Fs.dir_ty Eio.Path.t -> int
  (** [stats t ~dir] is the number of active callbacks registered for the
      directory [dir]. *)

  val apply :
    t -> dir:Eio.Fs.dir_ty Eio.Path.t -> file:Eio.Fs.dir_ty Eio.Path.t -> unit
  (** [apply t ~dir ~file] calls [f file] for every callback [f] registered for
      the directory [dir]. *)

  val add :
    t ->
    id:int ->
    dir:Eio.Fs.dir_ty Eio.Path.t ->
    (Eio.Fs.dir_ty Eio.Path.t -> unit) ->
    unit
  (** [add t ~id ~dir f] adds a new callback [f] to the directory [dir], using
      the unique identifier [id]. *)

  val remove : t -> id:int -> dir:Eio.Fs.dir_ty Eio.Path.t -> unit
  (** [remove t ~id ~dir] removes the callback with ID [id] on the directory
      [dir]. *)

  val length : t -> int
  (** [length t] is [t]'s length. *)
end

(** Watchdog functions. Ensure that only one background process is monitoring
    events for a given directory. *)
module Watchdog : sig
  type t
  (** The type for filesystem watchdogs. *)

  val dispatch : t -> Dispatch.t
  (** [dispath t] is the table of [t]'s callback dispatch. *)

  type hook = (Eio.Fs.dir_ty Eio.Path.t -> unit) -> unit -> unit
  (** The type for watchdog hook. *)

  val empty : unit -> t
  (** [empty ()] is the empty watchdog, monitoring no directory. *)

  val clear : t -> unit
  (** [clear ()] stops all the currently active watchdogs. *)

  val start : t -> dir:Eio.Fs.dir_ty Eio.Path.t -> hook -> unit
  (** [start t ~dir h] adds a new callback hook on the directory [dir], starting
      a new watchdog if needed otherwise re-using the previous one. *)

  val stop : t -> dir:Eio.Fs.dir_ty Eio.Path.t -> unit
  (** [stop t ~dir] stops the filesystem watchdog on directory [dir] (if any). *)

  val length : t -> int
  (** [length t] is the number of watchdog threads. *)
end

type hook =
  int ->
  Eio.Fs.dir_ty Eio.Path.t ->
  (Eio.Fs.dir_ty Eio.Path.t -> unit) ->
  unit ->
  unit
(** The type for Irmin.Watch hooks. *)

type t
(** The type for listeners. *)

val create : (Eio.Fs.dir_ty Eio.Path.t -> Watchdog.hook) -> t
(** [create h] is the Irmin watcher using the update hook [h]. *)

val watchdog : t -> Watchdog.t
(** [watchdog t] is [t]'s watchdog. *)

val hook : t -> hook
(** [hook t] is [t]'s hook. *)

(** {1 Helpers} *)

val stoppable : sw:Eio.Switch.t -> (unit -> unit) -> unit -> unit
(** [stoppable ~sw t] is a function [f] such that calling [f] will cancel the
    thread [t]. The switch passed to the function is attached to the forked
    fiber for [t]. *)

val default_polling_time : float ref

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
