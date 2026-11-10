(*---------------------------------------------------------------------------
   Copyright (c) 2016 Thomas Gazagnaire. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Eio
open Astring

let src = Logs.Src.create "irmin-watcher" ~doc:"Irmin watcher logging"

module Log = (val Logs.src_log src : Logs.LOG)

(* [sw] is cancelled: only raise the cancellation if the caller is cancelled
   too. *)
let switch_cancelled () =
  Fiber.check ();
  invalid_arg "Irmin_watcher: the switch is cancelled"

let spawn ~sw f =
  let ready, set_ready = Promise.create () in
  let stopped, set_stopped = Promise.create () in
  let stop () = ignore (Promise.try_resolve set_stopped ()) in
  let run () =
    Switch.run @@ fun sw ->
    f ~sw ~ready:(fun () -> ignore (Promise.try_resolve set_ready (Ok ())))
  in
  let started = ref false in
  Fiber.fork_daemon ~sw (fun () ->
      started := true;
      (try Fiber.first (fun () -> Promise.await stopped) run
       with e when not (Promise.is_resolved ready) -> (
         (* Report the failure to [spawn]'s caller. *)
         let bt = Printexc.get_raw_backtrace () in
         Promise.resolve set_ready (Error (e, bt));
         match e with
         | Cancel.Cancelled _ -> Printexc.raise_with_backtrace e bt
         | _ when Promise.is_resolved stopped ->
             (* The caller was cancelled and is not waiting any more. *)
             Log.err (fun l ->
                 l "watchdog failed while starting: %a" Fmt.exn_backtrace (e, bt))
         | _ -> ()));
      `Stop_daemon);
  (* [fork_daemon] runs nothing once [sw] is cancelled. *)
  if not !started then switch_cancelled ();
  match Promise.await ready with
  | Ok () -> stop
  | Error (Cancel.Cancelled _, _) -> switch_cancelled ()
  | Error (e, bt) -> Printexc.raise_with_backtrace e bt
  | exception (Cancel.Cancelled _ as e) ->
      let bt = Printexc.get_raw_backtrace () in
      stop ();
      Printexc.raise_with_backtrace e bt

module Digests = struct
  include Set.Make (struct
    type t = string * Digest.t

    let compare = compare
  end)

  let of_list l = List.fold_left (fun set elt -> add elt set) empty l
  let sdiff x y = union (diff x y) (diff y x)
  let digest_pp ppf d = Fmt.string ppf @@ Digest.to_hex d
  let pp_elt = Fmt.(Dump.pair string digest_pp)
  let pp ppf t = Fmt.(Dump.list pp_elt) ppf @@ elements t

  let files t =
    elements t |> List.map fst |> String.Set.of_list |> String.Set.elements
end

module Dispatch = struct
  (* [key] is unique to each callback, while several callbacks can have the
     same [id]. *)
  type callback = { key : int; id : int; f : string -> unit }

  type t = {
    callbacks : (string, callback list) Hashtbl.t;
    mutable next_key : int;
  }

  let empty () = { callbacks = Hashtbl.create 10; next_key = 0 }
  let clear t = Hashtbl.clear t.callbacks
  let find t dir = try Hashtbl.find t.callbacks dir with Not_found -> []
  let stats t ~dir = List.length (find t dir)

  (* call all the callbacks on the file *)
  let apply t ~dir ~file =
    Fiber.List.iter
      (fun { id; f; _ } ->
        Log.debug (fun f -> f "callback %d" id);
        (* A failing callback must not stop the watchdog, which other
           callbacks share. *)
        try f file with
        | Cancel.Cancelled _ as e ->
            Printexc.raise_with_backtrace e (Printexc.get_raw_backtrace ())
        | e ->
            let bt = Printexc.get_raw_backtrace () in
            Log.err (fun l ->
                l "callback %d failed on %s: %a" id file Fmt.exn_backtrace
                  (e, bt)))
      (find t dir)

  let add t ~id ~dir f =
    let key = t.next_key in
    t.next_key <- key + 1;
    Hashtbl.replace t.callbacks dir ({ key; id; f } :: find t dir);
    fun () ->
      match List.filter (fun c -> c.key <> key) (find t dir) with
      | [] -> Hashtbl.remove t.callbacks dir
      | callbacks -> Hashtbl.replace t.callbacks dir callbacks

  let length t = Hashtbl.fold (fun _ v acc -> acc + List.length v) t.callbacks 0
end

module Watchdog = struct
  type t = { t : (string, unit -> unit) Hashtbl.t; d : Dispatch.t }

  let length t = Hashtbl.length t.t
  let dispatch t = t.d

  type hook = (string -> unit) -> unit -> unit

  let empty () : t = { t = Hashtbl.create 10; d = Dispatch.empty () }

  let clear { t; d } =
    Hashtbl.iter (fun _dir stop -> stop ()) t;
    Hashtbl.clear t;
    Dispatch.clear d

  let start { t; d } ~dir listen =
    if not (Hashtbl.mem t dir) then
      (* Note: multiple threads can wait here *)
      let u = listen (fun file -> Dispatch.apply d ~dir ~file) in
      if Hashtbl.mem t dir then
        (* Note: someone else won the race, cancel our own thread
           to avoid avoid having too many wathdogs for [dir]. *)
        u ()
      else (
        Log.debug (fun f -> f "Start watchdog for %s" dir);
        Hashtbl.add t dir u)

  let stop { t; d } ~dir =
    match Hashtbl.find_opt t dir with
    | None -> ()
    | Some stop ->
        if Dispatch.stats d ~dir <> 0 then
          Log.debug (fun f -> f "Active callbacks are registered for %s" dir)
        else (
          Log.debug (fun f -> f "Stop watchdog for %s" dir);
          Hashtbl.remove t dir;
          stop ())
end

type hook = int -> Eio.Fs.dir_ty Eio.Path.t -> (string -> unit) -> unit -> unit
type t = { listen : hook; watchdog : Watchdog.t }

let watchdog t = t.watchdog
let hook t = t.listen

let create ~sw listen =
  let watchdog = Watchdog.empty () in
  (* No watchdog runs once [sw] is finished. *)
  Switch.on_release sw (fun () -> Watchdog.clear watchdog);
  let listen id path fn =
    (* The watchdog of [dir] may still be registered, but stopped with
       [sw]. *)
    if Option.is_some (Switch.get_error sw) then switch_cancelled ();
    let dir = Eio.Path.native_exn path in
    let d = Watchdog.dispatch watchdog in
    let active = ref true in
    (* [fn] is not called once the hook is removed, even for a change found
       earlier. *)
    let remove =
      Dispatch.add d ~id ~dir (fun file -> if !active then fn file)
    in
    let unlisten () =
      if !active then (
        active := false;
        remove ();
        Watchdog.stop watchdog ~dir)
    in
    (try Watchdog.start watchdog ~dir (listen path)
     with e ->
       let bt = Printexc.get_raw_backtrace () in
       unlisten ();
       Printexc.raise_with_backtrace e bt);
    unlisten
  in
  { listen; watchdog }

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
