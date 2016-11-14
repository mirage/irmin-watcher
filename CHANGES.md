### 0.2.0

- When using fsevents/inotify do not scan the whole tree everytime
  (#6, @samoht)
- Use realpath(3) on Linux and GetFullPathName on Windows to
  normalise the path to watch (#6, @samoht)
- inotify: close the inotify file descriptor when stopping the
  watch (#6. @samoht)
- inotify: fix the path of watched events (inotify uses relative
  patch, unless fsevents which uses absolute paths) (#6, @samoht)
- fix detection of removed files (#6, @samoht)

### 0.1.4

- Use osx-fsevents > 0.2.0 to avoid an fd leak when starting/stoping
  the main watch scheduler.

### 0.1.3

- Fix `uname` runtime checks on Windows

### 0.1.2

- Fix link issue when no inotify/fsevents backends are available
- Use topkg 0.7.8

### 0.1.1

- Fix link issue with the inotify backend

### 0.1.0

- Initial release