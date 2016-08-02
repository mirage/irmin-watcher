#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
#require "str"

open Topkg

let fsevents = Conf.with_pkg "fsevents"
let inotify  = Conf.with_pkg "inotify"

let pre c =
  let fsevents = Conf.value c fsevents in
  let inotify  = Conf.value c inotify in
  let requires =
    let fsevents = if fsevents then ["osx-fsevents.lwt"] else [] in
    let inotify  = if inotify  then ["inotify.lwt"] else []  in
    String.concat " " (fsevents @ inotify)
  in
  OS.File.read "pkg/META.in" >>= fun meta ->
  let meta = Str.global_replace Str.(regexp "%%REQUIRES%%") requires meta in
  OS.File.write "pkg/META" meta

let build = Pkg.build ~pre ()

let () =
  Pkg.describe ~build "irmin-watcher" @@ fun c ->
  let fsevents = Conf.value c fsevents in
  let _inotify  = Conf.value c inotify in
  Ok [
    Pkg.mllib "src/irmin-watcher.mllib";
    Pkg.mllib ~cond:fsevents "src/irmin-watcher-fsevents.mllib";
    (* Pkg.mllib ~cond:inotify "src/irmin-watcher-inotify.mllib"; *)
  ]
