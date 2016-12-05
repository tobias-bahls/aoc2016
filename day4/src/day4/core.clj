(ns day4.core
   (:require [clojure.string :as str])
   (:gen-class))

(defn freq-comparator [x y]
  (compare [(val y) (int (key x))]
           [(val x) (int (key y))]))

(defn calc-checksum [str]
  (->> str
       frequencies
       (sort freq-comparator)
       keys
       (filter (partial not= \-))
       (take 5)
       str/join))

(defn validate-checksum [room]
  (= (:checksum room)
     (calc-checksum (:encrypted room))))

(defn parse-line [line]
  (let [ matches (re-matches #"([a-z\-]+)\-(\d+)\[([a-z]+)\]" line)
         [_ encrypted sectorid checksum] matches]
    {:encrypted encrypted
     :sectorid  (read-string sectorid)
     :checksum  checksum}))

(defn parse-input [input]
  (map parse-line (str/split-lines input)))

(defn rotate-char [times chr]
  (let [alphabet-offset (- (int chr) (int \a))
        rotated         (+ alphabet-offset times)
        wrapped         (mod rotated 26)
        asciified       (+ wrapped (int \a))]
    (char asciified)))

(defn encrypt-char [times chr]
  (if (= chr \-) \  (rotate-char times chr)))

(defn rotate-string [str, times]
  (str/join (map (partial encrypt-char times) str)))

(defn decrypt-room [room]
  (assoc room
         :decrypted (rotate-string (:encrypted room) (:sectorid room))))

(defn process-a [input]
  (->> input
       (filter validate-checksum)
       (map :sectorid)
       (reduce +)))

(defn process-b [input]
  (->> input
       (filter validate-checksum)
       (map decrypt-room)
       (filter #(= (:decrypted %) "northpole object storage"))
       (map :sectorid)
       first))

(defn -main [& args]
 (let [input    (parse-input (slurp (first args)))
       result-a (process-a input)
       result-b (process-b input)]
   (println "First Part:" result-a)
   (println "Second Part:" result-b)))
