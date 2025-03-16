-- Built for web client from examples

(import "std" "web")

- Create a handler
(defn f (fn ln) (concat "Hello, " fn " " ln "!"))

- Build + serve
(serve
  $ routeGET "/hello" (list "fname" "lname") f
  $ server 2025)
