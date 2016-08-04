open Ocamlbuild_plugin

let main_file = "file:src/irmin_watcher.ml"

let dispatch (pkg, name) =
  let pp_have = S[A "-pp"; A ("cppo -U HAVE_" ^ name)] in
  flag [main_file; "ocamldep"; "with-" ^ pkg] pp_have;
  flag [main_file; "ocaml"; "compile"; "with-" ^ pkg] pp_have;
  let pp_not_have = S[A "-pp"; A ("cppo -D HAVE_" ^ name)] in
  flag [main_file; "ocamldep"; "without-" ^ pkg] pp_not_have;
  flag [main_file; "ocaml"; "compile"; "without-" ^ pkg] pp_not_have

let dispatch = function
| After_rules ->
    dispatch ("fsevents", "FSEVENTS");
    dispatch ("inotify" , "INOTIFY")
| _ -> ()

let () = Ocamlbuild_plugin.dispatch dispatch
