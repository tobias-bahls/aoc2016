(ns day8.core
   (:require [clojure.string :as str]
             [clojure.math.combinatorics :as cmb])
   (:gen-class))

(defn create-screen [w, h]
 (vec (take h (repeat
                (vec (take w (repeat false)))))))

(defn rotate-right [coll by]
  (let [size        (count coll)
        num-to-drop (- size by)]
    (vec (take size (drop num-to-drop (cycle coll))))))

(defn get-column [coll idx]
  (map #(nth % idx) coll))

(defn set-column [coll idx contents]
  (let [contents-with-index (map vector contents (range 0 (count contents)))]
    (reduce #(assoc-in %1 [(nth %2 1) idx] (nth %2 0)) coll contents-with-index)))

(defn pixel-str [pxl]
  (if pxl \# \.))

(defn screen-str [screen]
  (str/join \newline (map #(str/join (map pixel-str %)) screen)))

(defn count-lit [coll]
  (reduce + (map #(count (filter true? %))  coll)))

(defn print-screen [screen]
  (println (screen-str screen)))

(defn rect [screen, a, b]
  (let [to-light (cmb/cartesian-product (range 0 b) (range 0 a))]
    (reduce #(assoc-in %1 %2 true) screen to-light)))

(defn rotate-column [screen, col, by]
  (let [column-vec      (get-column screen col)
        rotated-vec     (rotate-right column-vec by)
        replaced-column (set-column screen col rotated-vec)]
    replaced-column))

(defn rotate-row [screen, row, by]
  (let [row-vec         (nth screen row)
        rotated-vec     (rotate-right row-vec by)
        replaced-row    (assoc screen row rotated-vec)]
    replaced-row))

(defn parse-regex [re str]
  (map read-string (drop 1 (re-matches re str))))

(defn parse-rect [str]
  { :type :rect :params (parse-regex #"rect (\d+)x(\d+)" str)})

(defn parse-rotate-row [str]
  { :type :rotate-row :params (parse-regex #"rotate row y=(\d+) by (\d+)" str)})

(defn parse-rotate-col [str]
  { :type :rotate-col :params (parse-regex #"rotate column x=(\d+) by (\d+)" str)})

(defmulti execute-instruction (fn [screen instruction] (:type instruction)))

(defmethod execute-instruction :rect [screen instruction]
  (let [params (:params instruction)]
    (rect screen (nth params 0) (nth params 1))))

(defmethod execute-instruction :rotate-row [screen instruction]
  (let [params (:params instruction)]
    (rotate-row screen (nth params 0) (nth params 1))))

(defmethod execute-instruction :rotate-col [screen instruction]
  (let [params (:params instruction)]
    (rotate-column screen (nth params 0) (nth params 1))))

(defn parse-input [input]
  (for [line (str/split-lines input)]
    (cond
      (str/starts-with? line "rect")          (parse-rect line)
      (str/starts-with? line "rotate row")    (parse-rotate-row line)
      (str/starts-with? line "rotate column") (parse-rotate-col line))))

(defn process-a [input]
  (count-lit (reduce execute-instruction (create-screen 50 6) input)))

(defn process-b [input]
  (str \newline (screen-str (reduce execute-instruction (create-screen 50 6) input))))

(defn -main [& args]
 (let [input    (parse-input (slurp (first args)))
       result-a (process-a input)
       result-b (process-b input)]
   (println "First Part:" result-a)
   (println "Second Part:" result-b)))
