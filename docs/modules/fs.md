# Filesystem

### Description
Module that allows you to interact with files.

### Import
```
(import "fs")
```

### Usage
```
(print $ readF "text.txt")
```

### Reference
- `(readF <string:filename>)` -- reads file as string and returns its text.
- `(writeF <string:filename> <string:contents>)` -- writes contents to file.
- `(appendF <string:filename> <string:contents>)` -- appends contents to file.
- `(lsdir)` -- returns list of strings -- files in CWD.
- `(lsdir <string:path>)` -- returns list of strings -- files in `path`.
- `(chdir <string:path>)` -- changes CWD to `path`.

### Source
[Source](https://github.com/ProggerX/lapse-hs/blob/master/src/Lapse/Modules/FS.hs)
