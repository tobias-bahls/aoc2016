(ns day18.core
  (:gen-class)
  (:require [clojure.string :as str]
            [digest :as digest]))

(defn apply-repeatedly [f input]
  (let [this (f input)]
    (lazy-seq (cons this ((partial apply-repeatedly f) this)))))

(defn is-trap? [char]
  (case char
    \^ true
    \. false
    :otherwise nil))

(defn parse-row [str]
  (mapv is-trap? str))

(defn get-new-tile [old-row idx]
  (let [left   (get old-row (dec idx) false)
        center (get old-row      idx  false)
        right  (get old-row (inc idx) false)]
    (or (and left  center       (not right))
        (and right center       (not left))
        (and left  (not center) (not right))
        (and right (not center) (not left)))))

(defn expand-row [row]
  (mapv #(get-new-tile row %1) (range 0 (count row))))

(def expand-row-seq (partial apply-repeatedly expand-row))

(defn- count-safe [input num-rows]
  (+ (count (filter false? input))
     (reduce +
             (map #(count (filter false? %))
                  (take (dec num-rows) (expand-row-seq input))))))

(defn process-a [input]
  (count-safe input 40))

(defn process-b [input]
  (count-safe input 400000))

(def raw-input ".^.^..^......^^^^^...^^^...^...^....^^.^...^.^^^^....^...^^.^^^...^^^^.^^.^.^^..^.^^^..^^^^^^.^^^..^")

(defn -main [& args]
  (let [input    (parse-row raw-input)
        result-a (process-a input)
        result-b (process-b input)]
    (println "First Part:" result-a)
    (println "Second Part:" result-b)))
