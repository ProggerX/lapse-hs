(import "std" "io" "fs")

(set name $ readF "name.txt")
(writeF "greeting.txt" "Hello, ")
(appendF "greeting.txt" name)
