(ns day3.core
   (:require [clojure.string :as str])
   (:gen-class))

(defn triangle? [[a b c]]
  (and (> (+ a b) c)
       (> (+ a c) b)
       (> (+ b c) a)))

(defn parse-input [input]
  (->> input
       str/split-lines
       (map #(str/split % #" "))
       (map #(filter (complement empty?) %))
       (map #(map read-string %))
       vec))

(defn process-a [input]
  (count (filter triangle? input)))

(defn transpose [coll]
  (map (fn [idx] (map #(nth % idx) coll)) (range 0 3)))

(defn process-b [lines]
  (process-a (->> lines
                  (partition 3 3)
                  (map transpose)
                  flatten
                  (partition 3 3))))

(defn -main [& args]
 (let [input    (parse-input (slurp (first args)))
       result-a (process-a input)
       result-b (process-b input)]
   (println "First Part:" result-a)
   (println "Second Part:" result-b)))
