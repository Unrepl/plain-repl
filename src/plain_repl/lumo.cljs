(ns plain-repl.lumo
  (:require [cljs.tools.reader :as r]
    [cljs.tools.reader.reader-types :as rt]
    [lumo.repl :as lumo]
    [cljs.analyzer :as ana]
    [cljs.env :as env]
    [cljs.tagged-literals :as tags]
    [cljs.js :as cljs]
    [clojure.string :as str]))

;; This file is a work in progress implementation of an upgradable REPL for lumo

(defprotocol AsyncReader
  "Asynchronous stream of strings."
  (read-chars [r f] "Calls f with a string or nil (EOF)")
  (pushback [r s] "Unread s"))

(def ^:dynamic ^{:doc "Must statisdy AsyncReader"} *in*)

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
      (reify AsyncReader
        (read-chars [_ f]
          (if-some [s (.pop ss)]
            (f s)
            (do
              (vreset! vf f)
              (.resume stream))))
        (pushback [r s]
          ; when performing the pushback there should be no pending read
          (when-not (= "" s) (.push ss s)))))))

; a cljs.reader reader that throws trying to read after the last character (unless eof is true)
; this allows to trap when the cljs reader needs more input to decide if a token is complete.
(deftype ThrowingStringPushbackReader [s vpos eof]
  rt/Reader
  (read-char [_]
    (let [pos @vpos]
      (if (< pos (.-length s))
        (let [ch (.charAt s pos)]
          (vreset! vpos (inc pos))
          ch)
       (when-not eof (throw (js/Error. "NEEDMORECHARS"))))))
  (peek-char [_]
    (let [pos @vpos]
      (if (< pos (.-length s))
        (.charAt s pos)
        (when-not eof (throw (js/Error. "NEEDMORECHARS"))))))
  rt/IPushbackReader
  (unread [_ ch]
    (vswap! vpos dec)
    #_(assert (= ch (.charAt s @vpos)))))

(defn- read* [opts async-reader cb buf]
  (let [vpos (volatile! 0)]
    (read-chars async-reader
      (fn [s]
        (let [at-eof (nil? s)
              buf (str buf s)
              rdr (ThrowingStringPushbackReader. buf vpos at-eof)]
          (vreset! vpos 0)
          (if-some [[v e] (try
                            [(r/read opts rdr) nil]
                            (catch :default e
                              (when-not (= (.-message e) "NEEDMORECHARS") ; can't get this error when at-eof
                                [nil e])))]
            (do
              (pushback async-reader (subs buf @vpos))
              (cb v e))
            (read* opts async-reader cb buf)))))))

(defn read
  "Asynchronous read that takes a (fn [value error] ...) as callback."
  ([cb] (read *in* cb))
  ([async-reader cb] (read {} async-reader cb))
  ([opts async-reader cb] (read* opts async-reader cb ""))
  ([async-reader eof-error? eof-value cb]
    (read {:eof (if eof-error? :eofthrow eof-value)} async-reader cb)))

(def ^:dynamic *session-id* 0)

(defn eval [form cb]
  (try
    (lumo/set-session-state-for-session-id! *session-id*)
    (binding [ana/*cljs-warning-handlers* [lumo/warning-handler]
              cljs/*eval-fn*   lumo/caching-node-eval
              cljs/*load-fn*   lumo/load
              ana/*cljs-ns*    @lumo/current-ns
              *ns*             (create-ns @lumo/current-ns)
              env/*compiler*   lumo/st
              r/resolve-symbol ana/resolve-symbol
              tags/*cljs-data-readers* (merge tags/*cljs-data-readers* (lumo/load-data-readers! env/*compiler*))
              r/*alias-map*    (lumo/current-alias-map)]
      (let [eval-opts (merge (lumo/make-eval-opts)
                        {:context :expr
                         :def-emits-var true})]
        (if (lumo/repl-special? form)
          ((get lumo/repl-special-fns (first form)) form eval-opts)
          (cljs/eval lumo/st form eval-opts
            (fn [{:keys [ns value error] :as ret}]
              (when-not error
                (do
                  (when (lumo/def-form? form)
                    (let [{:keys [ns name]} (meta value)]
                      (swap! lumo/st assoc-in [::ana/namespaces ns :defs name ::repl-entered-source] form))) ; pr-str of form?
                  (vreset! lumo/current-ns ns)))
              (cb value error))))))
    (catch :default e
      ;; `;;` and `#_`
      (when-not (identical? (.-message e) "Unexpected EOF.")
        (cb nil e)))
    (finally (lumo/capture-session-state-for-session-id *session-id*)))
  nil)

(defprotocol Suspension
  (then [s f])
  (resume [s v e]))

(defn suspension []
  (let [state (volatile! nil)]
    (letfn [(init
              ([f] (vreset! state (waiting f)))
              ([v e] (vreset! state (pending v e))))
            (waiting [f] (fn [v e] (vreset! state nil) (f v e)))
            (pending [v e] (fn [f] (vreset! state nil) (f v e)))]
      (vreset! state init)
      (reify Suspension
        (then [s f] (@state f))
        (resume [s v e] (@state v e))))))

(extend-protocol Suspension
  default
  (then [x f] (f x nil)))

(defn accept [socket]
  (let [in (bytes-stream-reader socket)
        print-fn #(.write socket (str/join " " %&))]
    (letfn [(repl [] (.write socket "=> ") (read in epl))
            (epl [form e]
              (binding [*in* in
                        *session-id* 42
                        *print-fn* print-fn]
                (if e
                  (do
                    (.write socket (prn-str e))
                    (repl))
                  (eval form pl))))
            (pl [v e] (then (or e v) l))
            (l [v e]
              (binding [*in* in
                        *session-id* 42
                        *print-fn* print-fn]
                (.write socket (prn-str (or e v)))
                (repl)))]
      (repl))))
