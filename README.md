# plain-repl

A Clojure library designed to ... well, that part is up to you.

## Usage

Start a lumo socket repl `lumo -c src/ -n '{"port": 5557, "accept": "plain-repl.lumo/accept"}'`.

Start a client `nc localhost 5557`.

In the clent type
```clj
(require '[plain-repl.lumo :as p])
(let [s (p/suspension)
      in p/*in*
      print-fn *print-fn*]
  ((fn echo []
     (binding [p/*in* in]
       (p/read (fn [form e]
                 (if (or e (= 'fin form))
                   (p/resume s form nil)
                   (binding [*print-fn* print-fn]
                     (prn 'you 'said form)
                     (echo))))))))
  s)
```

Now the repl should have been upgraded.

## License

Copyright © 2017 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
