(import "std" "io")

(set message "Running an example\n...")
(print message)

(write "Enter your name: ")
(flush)
(set name (getline))
(let ((message (concat "Hello, " name "!"))) (print message))

(defmacro if (c then else) (cond ((eval c) then) (1 else)))
(if (> 1 2) (print "1 > 2")
	  (print "1 <= 2"))
(if (< 1 2) (print "1 < 2")
	  (print "1 >= 2"))

(defn fac (x) (if (< x 1) 1 (* x (fac (- x 1)))))
(set fact6 (show (fac 6)))
(print (concat "Factorial of 6: " fact6))

(import "fibonacci.lp")
(set fib8 (show (fib 8)))
(print (concat "8-th Fibonacci number: " fib8))
