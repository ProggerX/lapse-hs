-- Built for web client from examples

(import "std" "web" "json")

(defn greet (fn ln) (concat "Hello, " fn " " ln "!"))

- Create a handler
(defn f (fn ln) (encode $ dict
  (first_name fn)
  (last_name ln)
  (greeting $ greet fn ln)))

- Build + serve
(serve
  $ routeGET "/greet" (list "fname" "lname") f
  $ server 2025)
