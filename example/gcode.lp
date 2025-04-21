(import "std" "io")

(set curx 0) (set cury 1)

(import "modules/gcode.lp")

(init "plra4")
(base)
(print $ concat "cury: " $ show cury)
(go 5 50)
(delta 5 40)
