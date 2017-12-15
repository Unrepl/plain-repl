(ns plain-repl.min)

;; ideally this namespace content should make its way to cljs itself

(defprotocol AsyncReader
  "Asynchronous stream of strings."
  (read-chars [r f] "Calls f with a string or nil (EOF)")
  (pushback [r s] "Unread s"))

(deftype Suspension [f])

(defn suspension? [x] (instance? Suspension x))

(defn suspend-host
  "Returns a repl suspension. 
   When an eval returns a suspension then the repl must yield control to the suspension and not print or loop until the suspension completes."
  [f]
  (Suspension. f))

(defn yield-control
  "Yields control to the suspension, passing it input and output.
   When the suspension completes it calls resume-host (a function of two arguments: value and exception)."
  [suspension async-reader resume-host]
  ((.-f suspension) async-reader resume-host))
