(ns day20.core
  (:gen-class)
  (:require [clojure.string :as str]))

(defn find-lowest
  ([ranges]
   (find-lowest 0 ranges))
  ([lowest [[lo hi] & ranges]]
   (if (> lo (inc lowest))
     (inc lowest)
     (recur (max lowest hi) ranges))))

(defn num-allowed
  ([ranges]
   (num-allowed 0 0 ranges))
  ([lowest num-allowed [[lo hi] & ranges]]
   (if lo
     (if (> lo (inc lowest))
       (recur (max lowest hi) (+ num-allowed (- lo lowest 1)) ranges)
       (recur (max lowest hi) num-allowed ranges))
     num-allowed)))

(defn parse-input [raw]
  (->> raw
       str/split-lines
       (mapv #(str/split % #"-"))
       (mapv #(mapv read-string %))
       (sort-by first)))

(defn process-a [input]
  (find-lowest input))

(defn process-b [input]
  (num-allowed input))

(defn -main [& args]
  (let [input    (parse-input (slurp (first args)))
        result-a (process-a input)
        result-b (process-b input)]
    (println "First Part:" result-a)
    (println "Second Part:" result-b)))
