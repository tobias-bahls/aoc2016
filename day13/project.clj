(defproject day13 "0.1.0-SNAPSHOT"
  :description "AoC 2016 Day 13"
  :url ""
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [proto-repl "0.3.1"]
                 [org.clojure/math.combinatorics "0.1.3"]
                 [org.clojure/data.priority-map "0.0.7"]]
  :main ^:skip-aot day13.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
