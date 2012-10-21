(defproject org.clojars.ejlo/regex-cljs "1.1.0-SNAPSHOT"
  :description "a DSL for people who prefer verbose, composable regexes. Clojurescript port of https://github.com/cgrand/regex"
  :url "http://github.com/ejlo/regex-cljs/"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "[1.4.0,)"]]
  :warn-on-reflection true

  :plugins [[lein-cljsbuild "0.2.8"]]

  :profiles {:dev     {:source-paths ["src/cljs-macros"]
                       :hooks [leiningen.cljsbuild]}
             :release {:source-paths ["src/cljs-macros"]}}

  :cljsbuild {:builds {:dev {:source-path "src/cljs"
                             :compiler {:output-to "target/js/regex-cljs.js"
                                        :output-dir "target/js/out"
                                        :optimizations nil
                                        :pretty-print true}}
                       :release {:source-path "src/cljs"
                                 :compiler {:output-to "target/js/regex-cljs.js"
                                            :optimizations :advanced}}}})
