(defproject org.clojars.franks42/fetch-repl "0.1.0-SNAPSHOT"
  :description "A ClojureScript Browser-REPL implementation based on noir and fetch."
  :url "https://github.com/franks42/fetch-repl"
  :license {:name "Eclipse Public License - v 1.0"
            :url "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo
            :comments "See the notice in README.md or details in COPYING"}
  
  :dependencies [
    [org.clojure/clojure "1.4.0"]
    [org.clojure/clojurescript "0.0-1552"]
    [cljsbuild "0.2.9"]
    [org.clojars.franks42/cljs-uuid-utils "0.1.3"]
    [noir "1.3.0"]
    [fetch "0.1.0-alpha2FS"]
    ]
  )
