### 0.6.0 (unreleased)

- Move from Lwt to Eio (#37, #46, @patricoferris, @clecat, @zshipko, @balat).
  This changes the API:
  - `hook`, the callbacks and the functions removing hooks are in direct
    style instead of returning `Lwt.t` values. `hook` must be called from an
    Eio fiber.
  - The watched directory is an `Eio.Fs.dir_ty Eio.Path.t` instead of a
    string. Callbacks still receive the path of the changed file relative to
    it.
  - `v` is now a function creating a watcher, `Irmin_watcher.v ~sw ()`, and
    there is no global watcher. `hook` and `stats` take the watcher as their
    first argument.
  - `set_polling_time` is replaced by the `?polling_time` argument of `v`,
    which raises `Invalid_argument` if it is not positive.
  - `hook` returns once the directory has been read, and raises the errors
    met while starting to watch it. It raises `Invalid_argument` once the
    switch of the watcher is cancelled.
  - Callbacks always receive paths with `/` as separator, also on Windows.
  - Watched directories are no longer resolved with `realpath`: a directory
    reached through two different paths gets two watchdogs.
  - Exceptions raised by callbacks are logged.
  - The FSevents backend (macOS) needs a running `Lwt_eio` event loop.
- Removing a hook twice no longer removes another hook with the same id.
- A callback is no longer called once its hook is removed.
- A directory removed while it is scanned no longer stops the watcher.
- Files that cannot be read keep their previous state and are read again
  later, instead of being reported as removed.
- Scan the watched directory once instead of twice when a hook starts.
- With Inotify, rescan the directory when a sub-directory changes or when the
  event queue overflows.
- Require OCaml 5.2 and Eio 1.6 or later.
- Depend on `lwt`, `cf-lwt` and `fsevents-lwt` only on macOS.

### 0.5.1 (2026-04-24)

- Use `_WIN32` and `MAX_PATH` on Windows to support MSVC (#34, @jonahbeckford)
- Fix build on Alpine 3.19 for OCaml 5.1 (#35, @shakthimaan)
- Update GitHub Actions workflow for macOS (#36, @art-w)
- Documentation: update links (#33, @gs0510) and fix `opam` link in README (#45, @ElectreAAS)

### 0.5.0 (2020-04-30)

- Switch to GitHub Actions from Travis (#31, @avsm)
- Initialise backends only when needed via a
  lazy watcher interface (#31, @samoht @avsm)
- Use fsevents and cf-lwt packages (#31, @avsm)
- Use ocamlformat.0.18.0 (#31, @avsm)

### 0.4.1 (2019-07-02)

- Clearer name for hook logger (@talex5, #21)
- Fix race when scanning directories (@talex5, #21)
- Make listen loop tail-recursive (@talex5, #21)

### 0.4.0 (2018-10-08)

- use dune (#13, @mc10)
- rename `unix_realpath` function name to avoid name clashes (#17, @samoht)

### 0.3.0 (2017-06-21)

- Use jbuilder (#11, @samoht)

### 0.2.0 (2016-11-14)

- Allow to watch non-existing directories (#8, @samoht)
- Expose `Irmin_watches.stats` to get stats about the numbers
  of active watchdogs, and callback dispatchers (#7, @samoht)
- When using fsevents/inotify do not scan the whole tree everytime
  (#6, @samoht)
- Use realpath(3) on Linux and GetFullPathName on Windows to
  normalise the path to watch (#6, @samoht)
- inotify: close the inotify file descriptor when stopping the
  watch (#6. @samoht)
- inotify: fix the path of watched events (inotify uses relative
  patch, unless fsevents which uses absolute paths) (#6, @samoht)
- fix detection of removed files (#6, @samoht)

### 0.1.4 (2016-08-16)

- Use osx-fsevents > 0.2.0 to avoid an fd leak when starting/stoping
  the main watch scheduler.

### 0.1.3 (2016-08-15)

- Fix `uname` runtime checks on Windows

### 0.1.2 (2016-08-10)

- Fix link issue when no inotify/fsevents backends are available
- Use topkg 0.7.8

### 0.1.1 (2016-08-09)

- Fix link issue with the inotify backend

### 0.1.0 (2016-08-09)

- Initial release
