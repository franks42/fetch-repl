;; Copyright (c) Frank Siebenlist. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns clj.repl.fetch-repl
  "Module provides a clojurescript repl-implementation based on the fetch and noir libraries.
  It is a functional replacement for the standard browser-repl that comes with the 
  clojurescript library. The advantages are that this repl re-uses the same port and connection
  that the browser uses to communicate with the noir-server. This fetch-repl uses the same
  plugin conventions as the clojurescript repl, which (almost) guarantees a seamless replacement.
  The repl-functionality is implemented with reverse fetch-calls from the browser's js-vm
  to the noir server. The setup is minimal as the fetch-remotes leverage the noir/ring middleware.
  Simply require'ing the repl.fetch-repl namespace should be enough to enable the starting of
  the fetch-repl's cljs-repl from within a normal clj-repl session.
  "
  (:refer-clojure :exclude [loaded-libs])
  (:use [noir.fetch.remotes :only [defremote]])
  (:require 
            [clj.repl.fetch-repl-utils :as u]
            [clojure.java.io :as io]
            [cljs.compiler :as comp]
            [cljs.closure :as cljsc]
            [cljs.repl :as repl]
            [cljs.repl.server :as server])
  (:import cljs.repl.IJavaScriptEnv))


;; initialize def's

(def ^:dynamic *the-repl-env*
  "The repl \"env\" that is used by to identify the repl-connection."
  nil)

(def ^:dynamic *deliver-delay* 
  "Artificially delay (ms) the printing of the repl-prompt to allow any concurrent 
  printing to stdio to come first." 
    100)

(def browser-window-id 
  "Will hold a map to associate *out* with request-id."
  (atom 0))

(def request-counter
  "request counter to id the individual requests&replies"
  (atom 0N))

(def request-waiting-light 
  "Incoming fetch-request waits on promise which is fulfilled when new work-item is added to queue."
  (atom (promise)))

(def ^:dynamic *request-waiting-light-time-out*
  "The request-calls from the js environment will time-out (ms) on the promise-atom
  request-waiting-light, and work as a heartbeat when no requests are pending."
  10000)

(def input-request-queue
  "queue for requests that have to be sent-to/picked-up-by browser
  normally will have js-code to be eval'ed in browser"
  (u/make-atomic-queue))

(def request-context-map
  "Maps :request-id value to a context map to associated 
  :result-promise when fetch-repl handler comes back with eval-result,
  it will lookup the promise to deliver to.
  Or :std-out to find the required redirection for printing."
  (atom {}))

(defn wrap-input-request
  "wrap request up in a map with annotations
  new requests will be enqueue'd like:
  (enqueue! input-request-queue (wrap-input-request req))"
  [req]
  {:request-id (u/next-count! request-counter) :req req})


;; functions


(defn pickup-eval-request
  "Handler will first see if there is an item waiting on the input-request-queue,
  if so, dequeue the item, reset the waiting-light and return the request.
  If not, wait on the promise until notified that new request is enqueued 
  or time-out, reset waiting-light and return the dequeued item or nil.
  Promise time-out can be used for heart-beat"
  []
  (if-let [req0 (if-let [req (u/dequeue! input-request-queue)]
                  (do
                    (reset! request-waiting-light (promise))
                    req)
                  (when (deref (deref request-waiting-light) 
                                *request-waiting-light-time-out* 
                                false)
                    (reset! request-waiting-light (promise))
                    (u/dequeue! input-request-queue)))]
    req0
    {:heart-beat true}))


(defn input-request-queue-watcher-handler
  "Handler will be called whenever the input-request-queue has changed,
  and will set the request-waiting-light promise to true if queue has items,
  such that any request handler can pick-up a possible new item."
  [_key _ref old-value new-value]
  (when (seq new-value)
    (deliver @request-waiting-light true)))

(add-watch input-request-queue 
           :input-request-queue-watcher-handler 
           input-request-queue-watcher-handler)


(defn send-js-for-eval
  "Puts request on queue, saves the *out* association, and returns the result-promise"
  [a-req]
  (let [the-request (wrap-input-request a-req)
        request-id (:request-id the-request)
        p (promise)]
    (u/enqueue! input-request-queue the-request)
    (swap! request-context-map assoc request-id {:result-promise p :std-out *out*})
    p))


(defn deliver-eval-result
  "Lookup the request-counter in the a-result map,
  and use that to find the associated promise in the request-context-map.
  Then remove that entry from the request-context-map,
  and finally deliver the result to the promise."
  [a-result]
  (Thread/sleep *deliver-delay*)
  (when-let [window-id (:window-id a-result)]
    (when (= @browser-window-id 0) 
      (reset! browser-window-id window-id)
      (reset! input-request-queue clojure.lang.PersistentQueue/EMPTY))
    (when-not (= window-id @browser-window-id)
      (reset! browser-window-id window-id)
      (map (fn [m] (deliver {:result-promise m} false)) (vals @request-context-map))
      (reset! request-context-map {}))
    (when-let [n (:request-id a-result)]
      (when-let [m (get @request-context-map n)]
        (when-let [p (:result-promise m)]
          (swap! request-context-map dissoc n)
          (deliver p (:reply a-result)))))))


(defremote reply-request [a-reply] 
  (deliver-eval-result a-reply)
  (let [a-request (pickup-eval-request)]
    a-request))

;;;;;;;;;;;;;;;


(defonce fetch-repl-state (atom {:return-value-fn nil
                              :client-js nil}))

(def loaded-libs (atom #{}))
(def preloaded-libs (atom #{}))

(defn- set-return-value-fn
  "Save the return value function which will be called when the next
  return value is received."
  [f]
  (swap! fetch-repl-state (fn [old] (assoc old :return-value-fn f))))

(defn send-for-eval
  "Given a form and a return value function, send the form to the
  browser for evaluation. The return value function will be called
  when the return value is received."
  ([form return-value-fn]
     (send-for-eval @(server/connection) form return-value-fn))
  ([conn form return-value-fn]
     (do (set-return-value-fn return-value-fn)
         (server/send-and-close conn 200 form "text/javascript"))))

(defn- return-value
  "Called by the server when a return value is received."
  [val]
  (when-let [f (:return-value-fn @fetch-repl-state)]
    (f val)))


(def ordering (agent {:expecting nil :fns {}}))

(defn add-in-order [{:keys [expecting fns]} order f]
  {:expecting (or expecting order) :fns (assoc fns order f)})

(defn run-in-order [{:keys [expecting fns]}]
  (loop [order expecting
         fns fns]
    (if-let [f (get fns order)]
      (do (f)
          (recur (inc order) (dissoc fns order)))
      {:expecting order :fns fns})))

(defn constrain-order
  "Elements to be printed in the REPL will arrive out of order. Ensure
  that they are printed in the correct order."
  [order f]
  (send-off ordering add-in-order order f)
  (send-off ordering run-in-order))


(defn browser-eval
  "Given a string of JavaScript, evaluate it in the browser and return a map representing the
   result of the evaluation. The map will contain the keys :type and :value. :type can be
   :success, :exception, or :error. :success means that the JavaScript was evaluated without
   exception and :value will contain the return value of the evaluation. :exception means that
   there was an exception in the browser while evaluating the JavaScript and :value will
   contain the error message. :error means that some other error has occured."
  [form]
  (let [return-value (send-js-for-eval form)]
    (let [ret (deref return-value 10000 :timed-out)]
      (if (= ret :timed-out)
        {:status :error
         :value "Eval-print timed out - result will be discarded"}
        (try (read-string ret)
             (catch Exception e
               {:status :error
                :value (str "Could not read return value: " ret)}))))))

(defn load-javascript
  "Accepts a REPL environment, a list of namespaces, and a URL for a
  JavaScript file which contains the implementation for the list of
  namespaces. Will load the JavaScript file into the REPL environment
  if any of the namespaces have not already been loaded from the
  ClojureScript REPL."
  [repl-env ns-list url]
  (let [missing (remove #(contains? @loaded-libs %) ns-list)]
    (when (seq missing)
      (browser-eval (slurp url))
      (swap! loaded-libs (partial apply conj) missing))))

(defrecord IFetchReplEnv []
  repl/IJavaScriptEnv
  (-setup [this]
    (do (require 'cljs.repl.reflect)
        (repl/analyze-source (:src this))
        (comp/with-core-cljs nil)))
  (-evaluate [_ _ _ js] (browser-eval js))
  (-load [this ns url] (load-javascript this ns url))
  (-tear-down [_] ;; not sure what to do here...
    ))

(defn compile-client-js [opts]
  (cljsc/build '[(ns clojure.browser.repl.client
                   (:require [goog.events :as event]
                             [clojure.browser.repl :as repl]))
                 (defn start [url]
                   (event/listen js/window
                                 "load"
                                 (fn []
                                   (repl/start-evaluator url))))]
               {:optimizations (:optimizations opts)
                :output-dir (:working-dir opts)}))

(defn create-client-js-file [opts file-path]
  (let [file (io/file file-path)]
    (when (not (.exists file))
      (spit file (compile-client-js opts)))
    file))

(defn- provides-and-requires
  "Return a flat list of all provided and required namespaces from a
  sequence of IJavaScripts."
  [deps]
  (flatten (mapcat (juxt :provides :requires) deps)))

(defn- always-preload
  "Return a list of all namespaces which are always loaded into the browser
  when using a browser-connected REPL."
  []
  (let [cljs (provides-and-requires (cljsc/cljs-dependencies {} ["clojure.browser.repl"]))
        goog (provides-and-requires (cljsc/js-dependencies {} cljs))]
    (disj (set (concat cljs goog)) nil)))

(defn repl-env
  "Create a fetch-repl-connected REPL environment.

  Options:

  port:           The port on which the REPL server will run. Defaults to 9000.
  working-dir:    The directory where the compiled REPL client JavaScript will
                  be stored. Defaults to \".repl\".
  serve-static:   Should the REPL server attempt to serve static content?
                  Defaults to true.
  static-dir:     List of directories to search for static content. Defaults to
                  [\".\" \"out/\"].
  preloaded-libs: List of namespaces that should not be sent from the REPL server
                  to the browser. This may be required if the browser is already
                  loading code and reloading it would cause a problem.
  optimizations:  The level of optimization to use when compiling the client
                  end of the REPL. Defaults to :simple.
  src:            The source directory containing user-defined cljs files. Used to
                  support reflection. Defaults to \"src/\".
  "
  [& {:as opts}]
  (let [opts (merge (IFetchReplEnv.)
                    {:optimizations :simple
                     :working-dir   ".repl"
                     :serve-static  true
                     :static-dir    ["." "out/"]
                     :preloaded-libs   []
                     :src           "src/"}
                    opts)]
    (do (reset! preloaded-libs (set (concat (always-preload) (map str (:preloaded-libs opts)))))
        (reset! loaded-libs @preloaded-libs)
        (swap! fetch-repl-state
               (fn [old] (assoc old :client-js
                               (future (create-client-js-file
                                        opts
                                        (io/file (:working-dir opts) "client.js"))))))
        opts)))


(defn run-repl-listen
  "Redefinition of lein-cljsbuild's run-repl-listen, such that we can set our own init-params 
  for fetch-repl's repl-env.
  Repl is started with: (run-repl-listen)"
  ([] (run-repl-listen 9000 ".lein-cljsbuild-repl"))
  ([port output-dir]
  (let [env (repl-env
              :port (Integer. port)
              :working-dir output-dir
              :src "src-cljs"
              :static-dir "resources/public"
              :serve-static true)]
    (def ^:dynamic *the-repl-env* env)
    (cljs.repl/repl env))))


;; RPCs to accommodate printing from the cljs-browser side.
;; 

(defremote fetch-print [msg]
  (let [request-id (:request-id msg)
        context-map (get @request-context-map request-id)]
    (if-let [std-out (:std-out context-map)]
      (binding [*out* std-out]
        (print (:print-string msg))(flush))
      (do (print (:print-string msg))(flush)))))


(defremote fetch-println [msg]
  (let [request-id (:request-id msg)
        context-map (get @request-context-map request-id)]
    (if-let [std-out (:std-out context-map)]
      (binding [*out* std-out]
        (println (:print-string msg))(flush))
      (do (println (:print-string msg))(flush)))))

(defremote doc [s] (with-out-str (cljs-info.doc/cljs-doc* s)))

(defremote clj2jvm [f] (eval f))

;; to sync my-atom on the jvm with cljs.user/my-atom on the js-vm:
;; (add-watch my-atom :my-atom-watcher (fn [_key _ref _old new-value] (println new-value)(cljs-info.repl/cljs->repl*  `(reset! cljs.user/my-atom ~new-value))))

