(defproject day12 "0.1.0-SNAPSHOT"
  :description "AoC 2016 Day 12"
  :url ""
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [proto-repl "0.3.1"]]
  :main ^:skip-aot day12.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
