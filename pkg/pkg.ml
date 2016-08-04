#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
#require "str"

open Topkg

let fsevents = Conf.with_pkg "fsevents"
let inotify  = Conf.with_pkg "inotify"

module Build = struct

  let pre c =
    let fsevents = Conf.value c fsevents in
    let inotify  = Conf.value c inotify in
    let requires =
      let fsevents = if fsevents then ["irmin-watcher.fsevents"] else [] in
      let inotify  = if inotify  then ["irmin-watcher.inotify"] else []  in
      String.concat " " (fsevents @ inotify)
    in
    OS.File.read "pkg/META.in" >>= fun meta ->
    let meta = Str.global_replace Str.(regexp "%%REQUIRES%%") requires meta in
    OS.File.write "pkg/META" meta

  let cppo c =
    let params = [
      fsevents, "fsevents";
      inotify , "inotify";
    ] in
    let one (p, n) = (if Conf.value c p then "with-" else "without-") ^ n in
    List.fold_left (fun acc x -> Cmd.(acc %% v "-tag" % one x)) Cmd.empty params

  (* the default build function *)
  let cmd c os files =
    let ocamlbuild = Conf.tool "ocamlbuild" os in
    let build_dir = Conf.build_dir c in
    let debug = Cmd.(on (Conf.debug c) (v "-tag" % "debug")) in
    OS.Cmd.run @@
    Cmd.(ocamlbuild % "-use-ocamlfind" % "-classic-display" %% debug %% cppo c %
         "-build-dir" % build_dir %% of_list files)

  let v = Pkg.build ~pre ~cmd ()

end

let () =
  Pkg.describe ~build:Build.v "irmin-watcher" @@ fun c ->
  let fsevents = Conf.value c fsevents in
  let _inotify  = Conf.value c inotify in
  Ok [
    Pkg.mllib "src/irmin-watcher.mllib";
    Pkg.mllib "src/irmin-watcher-core.mllib";
    Pkg.mllib "src/irmin-watcher-polling.mllib";
    Pkg.mllib ~cond:fsevents "src/irmin-watcher-fsevents.mllib";
    (* Pkg.mllib ~cond:inotify "src/irmin-watcher-inotify.mllib"; *)
    Pkg.test "test/test";
  ]
