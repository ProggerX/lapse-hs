# lapse-hs (WIP)
This will be a Lapse (my LISP dialect) interpreter in future.

Just an example:
```lapse
(set message "Running an example\n...")
(print message)
(print "Enter your name:")
(set name (getline))
(let ((message (concat "Hello, " name "!"))) (print message))
(set result (let ((a 1) (b 2) (c 3))
	'(a b c ,a ,b ,c (+ a b c) ,(+ a b c))
))
(print result)
(defmacro if (c then else) (cond ((eval c) then) (1 else)))
(if (> 1 2) (print "1 > 2")
	  (print "1 <= 2"))
(if (< 1 2) (print "1 < 2")
	  (print "1 >= 2"))
(defn fac (x) (if (< x 1) 1 (* x (fac (- x 1)))))
(print (fac 6))
```
