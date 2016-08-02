#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let fsevents = Conf.with_pkg "fsevents"

let () =
  Pkg.describe "irmin-watcher" @@ fun c ->
  let fsevents = Conf.value c fsevents in
  Ok [
    Pkg.mllib "src/irmin-watcher.mllib";
    Pkg.mllib ~cond:fsevents "src/irmin-watcher-fsevents.mllib";
  ]
