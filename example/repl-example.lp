(import "std" "io")

(defn repl ()
	(eval
		(write "(repl@lapse)>> ")
		(flush)
		(let ((ex (getline)))
			(print (eval (read ex)))
		)
		(flush)
		(repl)
	)
)

(repl)
