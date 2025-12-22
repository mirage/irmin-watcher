(lang dune 2.8)

(name irmin-watcher)

(generate_opam_files true)

(source
 (github mirage/irmin-watcher))

(license ISC)

(authors "Thomas Gazagnaire")

(maintainers "Thomas Gazagnaire")

(documentation "https://mirage.github.io/irmin-watcher/")

(package
 (name irmin-watcher)
 (synopsis "Portable Irmin watch backends using FSevents or Inotify")
 (description
  "irmin-watcher implements [Irmin's watch hooks][watch] for various OS,\nusing FSevents in macOS and Inotify on Linux.\n\nirmin-watcher is distributed under the ISC license.\n\n[watch]: http://mirage.github.io/irmin/irmin/Irmin/Private/Watch/index.html#type-hook\n")
 (depends
  (ocaml
   (>= "5.1.0"))
  (alcotest :with-test)
  (mtime
   (and
    :with-test
    (>= "2.0.0")))
  (inotify
   (and
    (>= "2.6")
    (= :os "linux")))
  (inotify-eio
   (and
    (>= "2.6")
    (= :os "linux")))
  (cf-lwt
   (>= "0.4"))
  lwt
  (eio
   (>= "1.0"))
  (eio_main
   (and
    :with-test
    (>= "1.0")))
  logs
  fmt
  astring
  fsevents-lwt
  (lwt_eio
   (or
    :with-test
    (= :os "macos")))))
