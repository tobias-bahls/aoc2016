(ns day19.core
  (:gen-class)
  (:require [clojure.string :as str]))

(defn left-neighbour-idx [num-elves idx _]
  (mod (inc idx) num-elves))

(defn get-next-neighbour-with-presents
  ([neighbour-fn idx elves]
   (get-next-neighbour-with-presents neighbour-fn (neighbour-fn idx elves) elves idx))
  ([neighbour-fn idx elves orig-idx]
   (cond
     (= idx orig-idx)       nil
     (contains? elves idx)  idx
     :otherwise             (recur neighbour-fn (neighbour-fn idx elves) elves orig-idx))))

(defn process-elves
  ([elves neighbour-fn]
   (process-elves elves 0 neighbour-fn))
  ([elves current neighbour-fn]
   (if (= 1 (count elves))
     current
     (let [neighbour  (get-next-neighbour-with-presents neighbour-fn current elves)]
       (cond
         (nil? neighbour) current
         :otherwise (do (-> elves
                            (assoc current (+ (get elves current)
                                              (get elves neighbour)))
                            (dissoc neighbour)
                            (recur (get-next-neighbour-with-presents
                                    neighbour-fn
                                    neighbour
                                    elves)
                                   neighbour-fn))))))))

(defn init-elves [count]
  (apply sorted-map (mapcat #(vector % 1) (range 0 count))))

(defn process-a [input]
  (let [elves           (init-elves input)
        neighbour-fn    (partial left-neighbour-idx (count elves))
        next-present-fn (partial get-next-neighbour-with-presents neighbour-fn)]
    (inc (process-elves elves next-present-fn))))

(defn process-b [input]
  (let [x (Math/pow 3 (int (/ (Math/log (- input 1)) (Math/log 3))))]
    (+ input
       (* x -1)
       (max 0 (- input (* 2 x))))))

(defn -main [& args]
  (let [result-a (process-a 3018458)
        result-b (process-b 3018458)]
    (println "First Part:" result-a)
    (println "Second Part:" result-b)))
