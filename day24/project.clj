(defproject day24 "0.1.0-SNAPSHOT"
  :description "AoC 2016 Day 24"
  :url ""
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/math.combinatorics "0.1.4"]] 
  :main ^:skip-aot day24.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
