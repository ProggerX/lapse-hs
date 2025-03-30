-- Built for web client from examples

(import "std" "web" "json")

(defn greet (fn ln) (concat "Hello, " fn " " ln "!"))

- Create a handler
(defn f (fn ln) (encode $ dict
  (first_name fn)
  (last_name ln)
  (greeting $ greet fn ln)))

(defn fpost (txt fn ln) (concat txt " " fn " " ln))

(defn echo (body) body)

- Build + serve
(serve
  $ routeGET "/greet" (list "fname" "lname") f
  $ routePOST "/post" (list "fname" "lname") fpost
  $ routePOST "/echo" () echo
  $ server 2025)
