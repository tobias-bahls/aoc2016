(defproject day17 "0.1.0-SNAPSHOT"
  :description "AoC 2016 Day 17"
  :url ""
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [digest "1.4.5"]
                 [org.clojure/data.priority-map "0.0.7"]]
  :main ^:skip-aot day17.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
