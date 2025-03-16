-- Uses web server from examples

(import "web" "std" "io")

- Build request
(set rget $ send
  $ withParam "fname" "First Name"
  $ withParam "lname" "Last Name"
  $ get "http://localhost:2025/hello")

- Print response
(print $ concat "Got response: " $ show rget)
(print "Body:")
(print $ resBody rget)
