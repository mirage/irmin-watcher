(*---------------------------------------------------------------------------
   Copyright (c) 2016 Thomas Gazagnaire. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Eio
open Astring

let src = Logs.Src.create "irmin-watcher" ~doc:"Irmin watcher logging"

module Log = (val Logs.src_log src : Logs.LOG)

(* run [t] and returns an handler to stop the task. *)
let stoppable ~sw t =
  let p, r = Promise.create () in
  Fiber.fork_daemon ~sw (fun () ->
      Fiber.first (fun () -> Promise.await p) t;
      `Stop_daemon);
  Promise.resolve r

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
  type t = (string, (int * (Eio.Fs.dir_ty Eio.Path.t -> unit)) list) Hashtbl.t

  let empty () : t = Hashtbl.create 10
  let clear t = Hashtbl.clear t

  let stats t ~dir =
    let nat_dir = Eio.Path.native_exn dir in
    try List.length (Hashtbl.find t nat_dir) with Not_found -> 0

  (* call all the callbacks on the file *)
  let apply t ~dir ~file =
    let nat_dir = Eio.Path.native_exn dir in
    let fns = try Hashtbl.find t nat_dir with Not_found -> [] in
    Fiber.List.iter
      (fun (id, f) ->
        Log.debug (fun f -> f "callback %d" id);
        f file)
      fns

  let add t ~id ~dir fn =
    let nat_dir = Eio.Path.native_exn dir in
    let fns = try Hashtbl.find t nat_dir with Not_found -> [] in
    let fns = (id, fn) :: fns in
    Hashtbl.replace t nat_dir fns

  let remove t ~id ~dir =
    let nat_dir = Eio.Path.native_exn dir in
    let fns = try Hashtbl.find t nat_dir with Not_found -> [] in
    let fns = List.filter (fun (x, _) -> x <> id) fns in
    if fns = [] then Hashtbl.remove t nat_dir else Hashtbl.replace t nat_dir fns

  let length t = Hashtbl.fold (fun _ v acc -> acc + List.length v) t 0
end

module Watchdog = struct
  type t = { t : (string, unit -> unit) Hashtbl.t; d : Dispatch.t }

  let length t = Hashtbl.length t.t
  let dispatch t = t.d

  type hook = (Eio.Fs.dir_ty Eio.Path.t -> unit) -> unit -> unit

  let empty () : t = { t = Hashtbl.create 10; d = Dispatch.empty () }

  let clear { t; d } =
    Hashtbl.iter (fun _dir stop -> stop ()) t;
    Hashtbl.clear t;
    Dispatch.clear d

  let watchdog t dir =
    let nat_dir = Eio.Path.native_exn dir in
    try Some (Hashtbl.find t nat_dir) with Not_found -> None

  let start { t; d } ~dir listen =
    match watchdog t dir with
    | Some _ -> assert (Dispatch.stats d ~dir <> 0)
    | None -> (
        (* Note: multiple threads can wait here *)
        let u = listen (fun file -> Dispatch.apply d ~dir ~file) in
        match watchdog t dir with
        | Some _ ->
            (* Note: someone else won the race, cancel our own thread
               to avoid avoid having too many wathdogs for [dir]. *)
            u ()
        | None ->
            Log.debug (fun f -> f "Start watchdog for %a" Eio.Path.pp dir);
            let nat_dir = Eio.Path.native_exn dir in
            Hashtbl.add t nat_dir u)

  let stop { t; d } ~dir =
    match watchdog t dir with
    | None -> assert (Dispatch.stats d ~dir = 0)
    | Some stop ->
        if Dispatch.stats d ~dir <> 0 then
          Log.debug (fun f ->
              f "Active callbacks are registered for %a" Eio.Path.pp dir)
        else (
          Log.debug (fun f -> f "Stop watchdog for %a" Eio.Path.pp dir);
          let nat_dir = Eio.Path.native_exn dir in
          Hashtbl.remove t nat_dir;
          stop ())
end

type hook =
  int ->
  Eio.Fs.dir_ty Eio.Path.t ->
  (Eio.Fs.dir_ty Eio.Path.t -> unit) ->
  unit ->
  unit

type t = {
  mutable listen :
    int ->
    Eio.Fs.dir_ty Eio.Path.t ->
    (Eio.Fs.dir_ty Eio.Path.t -> unit) ->
    unit;
  mutable stop : unit -> unit;
  watchdog : Watchdog.t;
}

let watchdog t = t.watchdog

let hook t id dir f =
  t.listen id dir f;
  t.stop

let create listen =
  let watchdog = Watchdog.empty () in
  let t = { listen = (fun _ _ _ -> ()); stop = (fun _ -> ()); watchdog } in
  let listen id dir fn =
    let d = Watchdog.dispatch watchdog in
    Dispatch.add d ~id ~dir fn;
    Watchdog.start watchdog ~dir (listen dir);
    let stop () =
      Dispatch.remove d ~id ~dir;
      Watchdog.stop watchdog ~dir
    in
    t.stop <- stop
  in
  t.listen <- listen;
  t

let default_polling_time = ref 1.

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
