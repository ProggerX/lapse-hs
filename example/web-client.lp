-- Uses web server from examples

(import "web" "std" "io" "json")

- Build request
(set rget $ send
  $ withParam "fname" "John"
  $ withParam "lname" "Black"
  $ get "http://localhost:2025/greet")

- Print response
(print $ concat "Got response: " $ show rget)
(print "Body:")
(print $ resBody rget)

- Decode JSON and get a greeting
(set gr $ lookup greeting $ fst $ decode $ resBody rget)

(print "Greeting:")
(print gr)
