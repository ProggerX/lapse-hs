(import "std" "io")

(set curx 100500)

(defn init (name)
	(cond
		((== name "plra4") (print "T1M6\nG17\n"))
		(1 (throw $ concat "I don't know this machine: " name))))

(defn base () (eval (print "G0 Z10\nG0 X0 Y0\n") (set curx 0) (set cury 0)))

(defn go (x y) (eval (print $ concat "G0 X" (show x) " Y" (show y) "\n") (set curx x) (set cury y)))

(defn delta (dx dy) (go (+ curx dx) (+ cury dy)))
