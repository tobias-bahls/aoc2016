(ns day2.core
   (:require [clojure.string :as str])
   (:gen-class))

(def relative-offset { \U {:x  0 :y -1}
                       \R {:x  1 :y  0}
                       \D {:x  0 :y  1}
                       \L {:x -1 :y  0}})

(def bounds [ {:lower 2 :upper 2}
              {:lower 1 :upper 3}
              {:lower 0 :upper 4}
              {:lower 1 :upper 3}
              {:lower 2 :upper 2}])

(def b-keypad [[\X \X  1 \X \X]
               [\X  2  3  4 \X]
               [5   6  7  8  9]
               [\X \A \B \C \X]
               [\X \X \D \X \X]])

(defn get-bounds [{:keys [x y]} type]
   { :x (get (nth bounds y) type)
     :y (get (nth bounds x) type)})

(defn num-at-a [{:keys [x y]}] (nth (range 1 10) (+ (* y 3) x)))
(defn num-at-b [{:keys [x y]}] (nth (nth b-keypad y) x))

(defn move-a [dir, pos]
  (->> pos
       (merge-with +   (get relative-offset dir))
       (merge-with max {:x 0 :y 0})
       (merge-with min {:x 2 :y 2})))

(defn move-b [dir, pos]
  (->> pos
       (merge-with +   (get relative-offset dir))
       (merge-with max (get-bounds pos :lower))
       (merge-with min (get-bounds pos :upper))))

(defn execute-sequence [pos move-fn [head & tail]]
  (if head
   (recur (move-fn head pos) move-fn tail)
   pos))

(defn process-code
  ([lines start-pos move-fn decode-fn]
   (process-code start-pos [] lines move-fn decode-fn))
  ([pos code [line & rest] move-fn decode-fn]
   (if line
    (let [new-pos (execute-sequence pos move-fn line)
          code    (conj code (decode-fn new-pos))]
      (recur new-pos code rest move-fn decode-fn))
    (str/join "" code))))

(defn process-code-a [lines]
  (process-code lines {:x 1 :y 1} move-a num-at-a))

(defn process-code-b [lines]
  (process-code lines {:x 0 :y 2} move-b num-at-b))

(defn -main [& args]
 (let [input    (str/split-lines (slurp (first args)))
       result-a (process-code-a input)
       result-b (process-code-b input)]
   (println "First Part:" result-a)
   (println "Second Part:" result-b)))
