open Ocamlbuild_plugin

let have_pkg pkg =
  run_and_read ("opam config var " ^ pkg ^ ":installed")
  |> String.trim
  |> bool_of_string

type t = {
  installed: bool;
  have     : string;
}

let inotify = {
  installed = have_pkg "inotify";
  have      = "INOTIFY";
}

let fsevents  = {
  installed = have_pkg "osx-fsevents";
  have      = "FSEVENTS";
}

let main_file = "file:src/irmin_watcher.ml"

let set_have t =
  let pp = match t.installed with
  | false -> S[A "-pp"; A ("cppo -U HAVE_" ^ t.have)]
  | true  -> S[A "-pp"; A ("cppo -D HAVE_" ^ t.have)]
  in
  flag [main_file; "ocamldep"] pp;
  flag [main_file; "ocaml"; "compile"] pp

let process t =
  Printf.printf "CONFIG: %s %b\n%!" t.have t.installed;
  set_have t

let dispatch = function
| After_rules -> process inotify; process fsevents
| _ -> ()

let () = Ocamlbuild_plugin.dispatch dispatch
