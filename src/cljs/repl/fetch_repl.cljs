(ns cljs.repl.fetch-repl
  "fetch-repl namespace for clojurescript side."
  (:require
    [cljs.reader]
    [clojure.browser.repl]
    [clojure.core.reducers]
    [fetch.remotes :as remotes]
    [cljs-uuid-utils]
    )
  (:require-macros [fetch.macros :as fm]))


(def ^:dynamic 
  *window-id*
  "Unique identifier (uuid) for the fetch-repl-session with this particular window.
  Will be set with a fresh, new uuid for each connection."
  nil)

(def ^:dynamic 
  *request-id*
  "Keep the request-id available as a context identifier that can be send with out-of-band
  requests like printing to use the correct stdout *out* on the clj-side."
  nil)

;; reverse fetch loop code

;; THE fetch-remote that those all the heavy lifting
(defn cljs-reply-request [a-reply] 
  (fetch.macros/remote (reply-request a-reply) [a-request]))

(def *keep-going* true)

(defn reply-request-loop
  "The basic reverse fetch loop that fetches the eval-requests from the noir-server,
  evals it locally in the browser's js-vm, and sends the result back to the server.
  Start the loop with (reply-request-loop)"
  ([] (reply-request-loop nil))
  ([a-reply]
    (when-not *window-id* (set! *window-id* (cljs-uuid-utils/make-random-uuid)))
    (let [a-reply (or a-reply {:request-id 0  :window-id *window-id*})]
      (fetch.macros/remote (reply-request a-reply) [a-request]
         (let [a-request a-request ;; (cljs.reader/read-string a-request)
               request-id (:request-id a-request)
               req-js-code (:req a-request)]
          (set! *request-id* request-id)
          (let [r (clojure.browser.repl/evaluate-javascript nil req-js-code)]
            (when *keep-going*
              (reply-request-loop {:request-id request-id :reply r :window-id *window-id*}))))))))


;; printing facility to stdout on the clj-jvm side

(defn remote-print [s]
  (let [msg {:print-string s :request-id *request-id*}]
    (fetch.macros/remote 
      (fetch-print msg) 
      [result] ;; discard returned result from print
      )))

(defn remote-println [s]
  (let [msg {:print-string s :request-id *request-id*}]
    (fetch.macros/remote 
      (fetch-println msg) 
      [result] ;; discard returned result from print
      )))

(set! *print-fn* remote-print)


(defn clj->jvm [forms]
  (fm/remote (clj2jvm forms) [result]
    (println "clj->jvm" result)
    result))

