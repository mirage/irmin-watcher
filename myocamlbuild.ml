open Ocamlbuild_plugin

let main_file = "file:src/irmin_watcher.ml"

let dispatch pkgs =
  let options =
    List.fold_left (fun acc (pkg, name) ->
        (List.map (fun (tags, flags) ->
             ("with-" ^ pkg ^ "-true") :: tags,
             ("-D HAVE_" ^ name) :: flags
           ) acc) @
        (List.map (fun (tags, flags) ->
             ("with-" ^ pkg ^ "-false") :: tags,
             ("-U HAVE_" ^ name) :: flags
           ) acc)
    ) [([], [])] pkgs
  in
  List.iter (fun (tags, flags) ->
      let flags = S[A "-pp"; A ("cppo " ^ String.concat " " flags)] in
      flag ([main_file; "ocamldep"] @ tags) flags;
      flag ([main_file; "ocaml"; "compile"] @ tags) flags
    ) options;
  flag ["file:src/irmin-watcher-core.cmxs"] (S[A"-I"; A"src"])

let dispatch = function
| After_rules -> dispatch [ ("fsevents", "FSEVENTS"); ("inotify" , "INOTIFY")]
| _ -> ()

let () = Ocb_stubblr.dispatchv [ Ocb_stubblr.init; dispatch ]
