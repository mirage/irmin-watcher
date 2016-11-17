#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"

open Topkg

let fsevents = Conf.with_pkg "fsevents"
let inotify  = Conf.with_pkg "inotify"

module Build = struct

  let pre c =
    let fsevents = Conf.value c fsevents in
    let inotify  = Conf.value c inotify in
    let requires =
      let xfsevents = if fsevents then ["irmin-watcher.fsevents"] else [] in
      let xinotify  = if inotify  then ["irmin-watcher.inotify"] else []  in
      let xpolling  =
        if not (fsevents || inotify) then [ "irmin-watcher.polling" ] else []
      in
      String.concat " " (xpolling @ xfsevents @ xinotify)
    in
    OS.File.read "pkg/META.in" >>= fun meta ->
    OS.File.write_subst "pkg/META" ["REQUIRES", requires] meta  >>= fun () ->
    let requires =
      if fsevents then "package(osx-fsevents.lwt), package(osx-cf.lwt), thread"
      else if inotify  then "package(inotify.lwt)"
      else "package(unix)"
    in
    OS.File.read "test/_tags.in" >>= fun tags ->
    OS.File.write_subst "test/_tags" ["REQUIRES", requires] tags

  let cppo c =
    let params = [
      fsevents, "fsevents";
      inotify , "inotify";
    ] in
    let one (p, n) = "with-" ^ n ^ "-" ^ string_of_bool (Conf.value c p) in
    List.fold_left (fun acc x -> Cmd.(acc %% v "-tag" % one x)) Cmd.empty params

  (* the default build function *)
  let cmd c os files =
    let ocamlbuild = Conf.tool "ocamlbuild" os in
    let build_dir = Conf.build_dir c in
    let debug = Cmd.(on (Conf.debug c) (v "-tag" % "debug")) in
    OS.Cmd.run @@
    Cmd.(ocamlbuild % "-use-ocamlfind" % "-classic-display" %% debug
         %% cppo c (* use cppo *)
         % "-plugin-tag" % "package(ocb-stubblr)" (* ocb-stubblr plugin *)
         % "-build-dir" % build_dir %% of_list files)

  let clean os ~build_dir =
    OS.Cmd.run @@ Pkg.clean_cmd os ~build_dir >>= fun () ->
    OS.File.delete "pkg/META" >>= fun () ->
    OS.File.delete "test/_tags"

  let v = Pkg.build ~pre ~cmd ~clean ()

end

let opams =
  let lint_deps_excluding = Some ["cppo"; "ocb-stubblr"] in
  [Pkg.opam_file ~lint_deps_excluding "opam"]

let metas = [Pkg.meta_file ~install:false "pkg/META.in"]

let () =
  Pkg.describe ~build:Build.v ~opams ~metas "irmin-watcher" @@
  fun c ->
  let fsevents = Conf.value c fsevents in
  let inotify  = Conf.value c inotify in
  Ok [
    Pkg.lib ~built:false "pkg/META";
    Pkg.clib  "src/librealpath.clib";
    Pkg.mllib "src/irmin-watcher.mllib";
    Pkg.mllib "src/irmin-watcher-core.mllib";
    Pkg.mllib "src/irmin-watcher-polling.mllib";
    Pkg.mllib ~cond:fsevents "src/irmin-watcher-fsevents.mllib";
    Pkg.mllib ~cond:inotify "src/irmin-watcher-inotify.mllib";
    Pkg.test "test/test" ~args:(Cmd.v "-e");
  ]
