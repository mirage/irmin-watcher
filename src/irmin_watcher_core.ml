(*---------------------------------------------------------------------------
   Copyright (c) 2016 Thomas Gazagnaire. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Astring
open Lwt.Infix

let src = Logs.Src.create "irmin-watcher" ~doc:"Irmin watcher logging"
module Logs = (val Logs.src_log src : Logs.LOG)

type t = int -> string -> (string -> unit Lwt.t) -> (unit -> unit) Lwt.t

(* run [t] and returns an handler to stop the task. *)
let stoppable t =
  let s, u = Lwt.task () in
  Lwt.async (fun () -> Lwt.pick ([s; t ()]));
  function () -> Lwt.wakeup u ()

let realdir dir =
  if Filename.is_relative dir then Filename.concat (Sys.getcwd ()) dir else dir

module Digests = struct
  include Set.Make(struct
      type t = string * Digest.t
      let compare = compare
    end)
  let of_list l = List.fold_left (fun set elt -> add elt set) empty l
  let sdiff x y = union (diff x y) (diff y x)
  let digest_pp ppf d = Fmt.string ppf @@ Digest.to_hex d
  let pp ppf t = Fmt.(Dump.list (Dump.pair string digest_pp)) ppf @@ elements t
  let files t =
    elements t |> List.map fst |> String.Set.of_list |> String.Set.elements
end

module Callback = struct

  type t = (string, (int * (string -> unit Lwt.t)) list) Hashtbl.t

  let empty (): t = Hashtbl.create 10
  let clear t = Hashtbl.clear t

  let stats t ~dir =
    try List.length (Hashtbl.find t dir) with Not_found -> 0

  (* call all the callbacks on the file *)
  let apply t ~dir ~file =
    let fns = try Hashtbl.find t dir with Not_found -> [] in
    Lwt_list.iter_p (fun (id, f) ->
        Logs.debug (fun f -> f "callback %d" id); f file
      ) fns

  let add t ~id ~dir fn =
    let fns = try Hashtbl.find t dir with Not_found -> [] in
    let fns = (id, fn) :: fns in
    Hashtbl.replace t dir fns

  let remove t ~id ~dir =
    let fns = try Hashtbl.find t dir with Not_found -> [] in
    let fns = List.filter (fun (x,_) -> x <> id) fns in
    if fns = [] then Hashtbl.remove t dir
    else Hashtbl.replace t dir fns

end

module Watchdog = struct

  type t = {
    t: (string, unit -> unit) Hashtbl.t;
    c: Callback.t;
  }

  let callback t = t.c

  type hook = (string -> unit Lwt.t) -> (unit -> unit) Lwt.t

  let empty (): t = {
    t = Hashtbl.create 10;
    c = Callback.empty ();
  }

  let clear { t; c } =
    Hashtbl.iter (fun _dir stop -> stop ()) t;
    Hashtbl.clear t;
    Callback.clear c

  let watchdog t dir =
    try Some (Hashtbl.find t dir) with Not_found -> None

  let start { t; c } ~dir listen =
    match watchdog t dir with
    | Some _ -> assert (Callback.stats c ~dir <> 0); Lwt.return_unit
    | None   ->
        (* Note: multiple threads can wait here *)
        listen (fun file -> Callback.apply c ~dir ~file) >|= fun u ->
        match watchdog t dir with
        | Some _ ->
            (* Note: someone else won the race, cancel our own thread
               to avoid avoid having too many wathdogs for [dir]. *)
            u ()
        | None   ->
            Logs.debug (fun f -> f "Start watchdog for %s" dir);
            Hashtbl.add t dir u

  let stop { t; c } ~dir =
    match watchdog t dir with
    | None      -> assert (Callback.stats c ~dir = 0)
    | Some stop ->
        if Callback.stats c ~dir = 0 then (
          Logs.debug (fun f -> f "Stop watchdog for %s" dir);
          Hashtbl.remove t dir;
          stop ()
        )
end

let create t listen =
  Watchdog.clear t;
  let listen_dir id dir fn =
    let dir = realdir dir in
    let c = Watchdog.callback t in
    Callback.add c ~id ~dir fn;
    Watchdog.start t ~dir (listen dir) >|= fun () ->
    function () ->
      Callback.remove c ~id ~dir;
      Watchdog.stop t ~dir
  in
  listen_dir

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
