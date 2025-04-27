# ANSI terminal colors

### Description
Small module that allows you to type thing like `(blue "hello")` and produces string with that color (e.g. `"\e[0;34mhello\e[0m"`).

### Import
```
(import "colors")
```

### Usage
```
(print $ blue "Hello, user!")
```

### Reference
- `(black <string>)` -- produces black string.
- `(red <string>)` -- produces red string.
- `(green <string>)`  -- produces green string.
- `(yellow <string>)` -- produces yellow string.
- `(blue <string>)` -- produces blue string.
- `(purple <string>)` -- produces purple string.
- `(cyan <string>)` -- produces cyan string.
- `(white <string>)`  -- produces white string.

### Source
[Source](https://github.com/ProggerX/lapse-hs/blob/master/modules/colors.lp)
