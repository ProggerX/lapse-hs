(import "std" "io" "colors")

(set machines $ dict)

(defmacro machine (name init end) (insert name '(,init . ,end) machines))

(machine "plra4"
  (do (print "T1M6\nG17\n") (set speed 400) (set mat -6.5))
  (print "M30"))

(defn init (name)
	(eval (set name name) (cond
		((== (lookup name machines) ()) (throw $ concat "I don't know this machine: " name ".\n" $ blue "Hint: you can use 'machine' to define a machine: (machine name init_func end_func)"))
    (1 (do (fst $ lookup name machines))))))

(defn base () (eval (print "G0Z10\nG0X0Y0\n") (set curx 0) (set cury 0) (set is_cutting ())))

(defn down () (eval (print $ concat "G1Z" (show mat) "F" (show speed) "\n") (set is_cutting 1)))

(defn up () (eval (print $ concat "G0Z10\n") (set is_cutting ())))

(defn go (x y) (eval 
    (cond
      (is_cutting (print $ concat "G1X" (show x) "Y" (show y) "F" (show speed) "\n"))
      (1 (print $ concat "G0X" (show x) "Y" (show y) "\n")))
    (set curx x) (set cury y)))

(defn delta (dx dy) (go (+ curx dx) (+ cury dy)))

(defn left (dx) (go (- curx dx) cury))
(defn right (dx) (go (+ curx dx) cury))
(defn forward (dy) (go curx (+ cury dy)))
(defn backward (dy) (go curx (- cury dy)))

(defn end () (do (snd $ lookup name machines)))
