(defproject day8 "0.1.0-SNAPSHOT"
  :description "AoC 2016 Day 8"
  :url ""
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"] [proto-repl "0.3.1"] [org.clojure/math.combinatorics "0.1.3"]]
  :main ^:skip-aot day8.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
