(import "std")

(defmacro if (c then else) (cond ((eval c) then) (1 else)))
(defn fib (a)
	(if (< a 2)
		a
		(+ (fib (- a 1)) (fib (- a 2)))
	)
)
