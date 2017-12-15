(ns plain-repl.repl
  (:require 
    [plain-repl.min :as min]
    [cljs.tools.reader :as r]
    [cljs.tools.reader.reader-types :as rt]
    [cljs.js :as cljs]))

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
    (min/read-chars async-reader
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
              (min/pushback async-reader (subs buf @vpos))
              (cb v e))
            (read* opts async-reader cb buf)))))))

(defn read
  "Asynchronous read that takes a (fn [value error] ...) as callback."
  ([async-reader cb] (read {} async-reader cb))
  ([opts async-reader cb] (read* opts async-reader cb ""))
  ([async-reader eof-error? eof-value cb]
    (read {:eof (if eof-error? :eofthrow eof-value)} async-reader cb)))

(defn skip-if-eol
  "If the next character on stream s is a newline, skips it, otherwise
  leaves the stream untouched. Returns :line-start, :stream-end, or :body
  to indicate the relative location of the next character on s. The stream
  must either be an instance of LineNumberingPushbackReader or duplicate
  its behavior of both supporting .unread and collapsing all of CR, LF, and
  CRLF to a single \\newline."
  [async-reader cb]
  (min/read-chars async-reader
    (fn [s]
      (cond
        (nil? s) (cb :stream-end)
        (= \newline (.charAt s 0)) (do (min/pushback async-reader (subs s 1)) (cb :line-start))
        :else (do (min/pushback async-reader s) (cb :body))))))

(defn skip-whitespace
  "Skips whitespace characters on stream s. Returns :line-start, :stream-end,
  or :body to indicate the relative location of the next character on s.
  Interprets comma as whitespace and semicolon as comment to end of line.
  Does not interpret #! as comment to end of line because only one
  character of lookahead is available. The stream must either be an
  instance of LineNumberingPushbackReader or duplicate its behavior of both
  supporting .unread and collapsing all of CR, LF, and CRLF to a single
  \\newline."
  [async-reader cb]
  (min/read-chars async-reader
    (fn self [s]
      (if (nil? s)
        (cb :stream-end)
        (let [[_ comment next-line next-form] (re-matches #"[\s,]*?(;[^\n]*)?(?:\n([^]*)|([^\s,][^]*))?" s)]
          (cond
            next-line
            (do
              (min/pushback async-reader next-line)
              (cb :line-start))
            comment
            (min/read-chars async-reader
              (fn skip-line [s]
                (if (nil? s)
                  (cb :stream-end)
                  (if-some [[_ next-line] (re-matches #".*?\n(.*)" s)]
                    (do
                      (min/pushback async-reader next-line)
                      (cb :line-start))
                    (min/read-chars async-reader skip-line)))))
            :else
            (do
              (min/pushback async-reader next-form)
              (cb :body))))))))

(defn repl-read
  "Default :read hook for repl. Reads from *in* which must either be an
  instance of LineNumberingPushbackReader or duplicate its behavior of both
  supporting .unread and collapsing all of CR, LF, and CRLF into a single
  \\newline. repl-read:
    - skips whitespace, then
      - returns request-prompt on start of line, or
      - returns request-exit on end of stream, or
      - reads an object from the input stream, then
        - skips the next input character if it's end of line, then
        - returns the object."
  [request-prompt request-exit in cb]
  (skip-whitespace in
    (fn [x]
      (if-some [v ({:line-start request-prompt :stream-end request-exit} x)]
        (cb v nil)
        (read {:read-cond :allow} in
          (fn [form]
            (skip-if-eol in (fn [_] (cb form)))))))))

(defn repl-prompt []
  (print (str (ns-name *ns*) "=> ")))

(defn repl-caught
  "Default :caught hook for repl"
  [e cb]
  (binding [*print-fn* *print-err-fn*]
    (println e))
  (cb false))

(defn repl-cr
  ([]
    {:print-fn *print-fn*
     :print-err-fn *print-err-fn*
     :ns *ns*
     :star1 *1
     :star2 *2
     :star3 *3
     :stare *e})
  ([{:keys [in print-fn print-err-fn ns star1 star2 star3 stare]}]
    (set! *print-fn* print-fn)
    (set! *print-err-fn* print-err-fn)
    (set! *ns* ns)
    (set! *1 star1)
    (set! *2 star2)
    (set! *3 star3)
    (set! *e stare)))

(defn repl [& {:keys [init need-prompt prompt flush read print cr]
               eval' :eval
               caught' :caught
               :or {init        #()
                    #_#_need-prompt #(identity true)
                    prompt      repl-prompt
                    #_#_flush       flush
                    read        repl-read
                    print       prn
                    caught      repl-caught}}]
  (let [cr (if cr
             (fn
               ([] [(repl-cr) (cr)])
               ([[repl-env env]] (repl-cr repl-env) (cr env)))
             repl-cr)
        env (volatile! (cr))
        request-prompt #js {}
        request-exit #js {}]
    (min/suspend-host
     (fn [in resume-host]
       (cr @env)
       (init)
       (letfn [(repl []
                 (prompt)
                 (read request-prompt request-exit in epl))
               (epl [form e]
                 (cr @env)
                 (cond
                   e (caught e)
                   (= request-prompt form) (repl)
                   (= request-exit form) (resume-host nil nil)
                   :else 
                   (eval' form then-pl))) ; todo *1 *2 *3
               (then-pl [v e]
                 (if (min/suspension? v)
                   (let [env (cr)] ; save env before yielding control
                     (min/yield-control v in (fn [v e] (cr env) (pl v e))))
                   (pl v e)))
               (pl [v e]
                 ; pl assumes to be called in a set up env
                 (if e
                   (caught e)
                   (when
                     (try
                       (print v)
                       true
                       (catch :default e
                         (caught e)
                         false)
                       (finally
                         (set! *3 *2)
                         (set! *2 *1)
                         (set! *1 v)
                         (vreset! env (cr))))
                     (repl))))
               (rrepl []
                 (cr @env)
                 (repl))
               (caught [e]
                 (set! *e e)
                 (caught' e rrepl))]
         (repl))))))

