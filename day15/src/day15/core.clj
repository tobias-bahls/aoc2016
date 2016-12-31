(ns day15.core
   (:require [clojure.string :as str])
   (:require [clojure.pprint :refer [pprint]])
   (:gen-class))

(defn disk [positions initial]
  (fn [time]
    (mod (+ initial time) positions)))

(defn disk-collection [& diskfns]
  (fn [time]
    (map #(%1 (+ time %2)) diskfns (range 1 (inc (count diskfns))))))

(defn solve [collection]
  (count
   (take-while #(not (every? (partial = 0) %))
                (map collection (range)))))

(defn process-a []
  (solve
   (disk-collection
    (disk  5  2)
    (disk 13  7)
    (disk 17 10)
    (disk  3  2)
    (disk 19  9)
    (disk  7  0))))

(defn process-b []
  (solve
   (disk-collection
    (disk  5  2)
    (disk 13  7)
    (disk 17 10)
    (disk  3  2)
    (disk 19  9)
    (disk  7  0)
    (disk 11  0))))

(defn -main [& args]
  (let [result-a (process-a)
        result-b (process-b nil)]
    (println "First Part:" result-a)
    (println "Second Part:" result-b)))
