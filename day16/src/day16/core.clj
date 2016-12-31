(ns day16.core
   (:require [clojure.string :as str])
   (:require [clojure.pprint :refer [pprint]])
   (:gen-class))

(defn dragon [input]
  (str input
       "0"
       (str/join
        (map #(if (= % \0) \1 \0)
             (str/reverse input)))))

(defn apply-repeatedly [f input]
  (let [this (f input)]
    (lazy-seq (cons this ((partial apply-repeatedly f) this)))))

(defn dragon-seq [input]
  (apply-repeatedly dragon input))

(defn data-of-length [initial size]
  (subs
   (first (take 1 (drop-while #(< (count %) size) (dragon-seq initial))))
   0
   size))

(defn checksum [input]
  (str/join
   (map
    (fn [[a b]] (if (= a b) "1" "0"))
    (partition 2 (str/split input #"")))))

(defn checksum-seq [input]
  (apply-repeatedly checksum input))

(defn calc-checksum [str]
  (first (take 1 (drop-while #(even? (count %)) (checksum-seq str)))))

(defn process-a [input]
 (calc-checksum (data-of-length input 272)))

(defn process-b [input]
 (calc-checksum (data-of-length input 35651584)))

(defn -main [& args]
  (let [result-a (process-a "10011111011011001")
        result-b (process-b "10011111011011001")]
    (println "First Part:" result-a)
    (println "Second Part:" result-b)))
