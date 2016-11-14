(*---------------------------------------------------------------------------
   Copyright (c) 2016 Thomas Gazagnaire. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

let uname () =
  try
    let ic = Unix.open_process_in "uname" in
    let uname = input_line ic in
    let () = close_in ic in
    Some uname
  with Unix.Unix_error _ ->
    None

let _is_linux () =
  Sys.os_type = "Unix" && uname () = Some "Linux"

let hook =
#ifdef HAVE_FSEVENTS
  let _ = uname in
  Irmin_watcher_fsevents.hook
#elif defined HAVE_INOTIFY
  if _is_linux () then
    Irmin_watcher_inotify.hook
  else
    Irmin_watcher_polling.(hook !default_polling_time)
#else
  Irmin_watcher_polling.(hook !default_polling_time)
#endif

let mode =
#ifdef HAVE_FSEVENTS
  `FSEvents
#elif defined HAVE_INOTIFY
  if _is_linux () then `Inotify else `Polling
#else
  `Polling
#endif

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
