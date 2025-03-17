(import "std" "json" "io")

(write "Enter json: ")
(flush)
(set j $ getline)

(print "You entered:")
(print j)

(print "Here is parsed result:")
(print $ decode j)
