(ns plain-repl.min.lumo
  (:require [plain-repl.min :as min]))

;; this file is the lumo (well, node) specific part

(defn bytes-stream-reader
  "Returns an AsyncReader from a node stream of buffers."
  ([stream] (bytes-stream-reader stream "UTF-8"))
  ([stream encoding]
    (let [ss #js []
          vf (volatile! nil)]
      (doto stream
        .pause
        (.on "data"
          (fn [buf]
            (.pause stream)
            (let [f @vf]
              (vreset! vf nil)
              (f (.toString buf encoding)))))
        (.on "end"
          (fn []
            (.pause stream)
            (let [f @vf]
              (vreset! vf nil)
              (f nil)))))
      (reify min/AsyncReader
        (read-chars [_ f]
          (if-some [s (.pop ss)]
            (f s)
            (do
              (vreset! vf f)
              (.resume stream))))
        (pushback [r s]
          ; when performing the pushback there should be no pending read
          (when-not (= "" s) (.push ss s)))))))
