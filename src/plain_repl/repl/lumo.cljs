(ns plain-repl.repl.lumo
  (:require [clojure.string :as str]
    [plain-repl.repl :as repl]
    [plain-repl.min :as min]
    [lumo.repl :as lumo]
    [cljs.analyzer :as ana]
    [cljs.env :as env]
    [cljs.tagged-literals :as tags]
    [cljs.tools.reader :as r]
    [cljs.js :as cljs]))

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

(def ^:dynamic *session-id* 0)

(defn eval [form cb]
  ; code heavily inspired from lumo/execute-text, some parts should go in read
  (try
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
                  (vreset! lumo/current-ns ana/*cljs-ns*)
                  (set! *ns* (create-ns @lumo/current-ns))))
              (cb value error))))))
    (catch :default e
      ;; `;;` and `#_`
      (when-not (identical? (.-message e) "Unexpected EOF.")
        (cb nil e))))
  nil)

(defn lumo-cr [session-id]
  (fn 
    ([] (lumo/capture-session-state-for-session-id session-id) nil)
    ([_] (lumo/set-session-state-for-session-id! session-id))))

(defn accept [socket]
  (binding [*print-fn* #(.write socket (str/join " " %&))
            *print-err-fn* *print-fn*
            *ns* (create-ns 'cljs.user)]
    (min/yield-control (repl/repl :eval eval :cr (lumo-cr 42))
      (bytes-stream-reader socket) (fn [_ _] (.end socket)))))