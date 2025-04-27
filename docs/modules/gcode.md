# G-code generator

### Description
Module that allows you to generate G-code from human-readable LISP code and prints it to stdout.

### Import
```
(import "gcode")
```

### Usage
```
(import "gcode")

(init "plra4")
(base)
(go 5 50)
(delta 5 -40)
(down)
(left 5)
(right 20)
(forward 50)
(right 20)
(backward 15)
(base)
(end)
```

### Reference
- `(init <string:name>)` -- init G-code for machine with name `name` (runs `init_func` of machine).
- `(machine <string:name> <function:init_func> <function:end_func>)` -- creates custom machine with name `name`, init `init_func` and end `end_func`.
- `(end)` -- end of G-code (runs `end_func` of machine).
- `(base)` -- go to base (`G0Z10`; `G0X0Y0`; disable cutting).
- `(down)` -- enable cutting and run `G1Z<material-z>F<speed>`.
- `(up)` -- disable cutting and run `G0Z10`.
- `(go x y)` -- go to x, y (`G0X<x>Y<y>` or `G1X<x>Y<y>F<speed>`).
- `(delta x y)` -- same as go, but relative to current position.
- `left, right, forward, backward` -- extra functions on top of delta.

### Source
[Source](https://github.com/ProggerX/lapse-hs/blob/master/modules/gcode.lp)
