(ns day6.core
   (:require [clojure.string :as str])
   (:gen-class))

(defn transpose [coll]
  (map (fn [idx] (map #(nth % idx) coll)) (range 0 (count (first coll)))))

(defn parse-input [input]
  (map #(str/split % #"") (str/split-lines input)))

(defn map-max [map]
  (key (apply max-key val map)))

(defn map-min [map]
  (key (apply min-key val map)))

(defn process [input map-fn]
  (let [freqs (map frequencies (transpose input))
        maxes (map map-fn freqs)]
    (str/join maxes)))

(defn process-a [input]
  (process input map-max))

(defn process-b [input]
  (process input map-min))

(defn -main [& args]
 (let [input    (parse-input (slurp (first args)))
       result-a (process-a input)
       result-b (process-b input)]
   (println "First Part:" result-a)
   (println "Second Part:" result-b)))
