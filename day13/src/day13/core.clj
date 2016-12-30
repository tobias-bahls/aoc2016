(ns day13.core
   (:require [clojure.string :as str])
   (:require [clojure.pprint :refer [pprint]])
   (:require [clojure.math.combinatorics :as cmb])
   (:require [clojure.data.priority-map :refer [priority-map]])
   (:gen-class))

(def dirs [[1 0] [-1 0] [0 1] [0 -1]])

(defn wall-formula [[x y] seed]
  (+ (* x x) (* 3 x) (* 2 x y) y (* y y) seed))

(defn ones-count [num]
  (count (filter (partial = "1") (str/split  (Integer/toString num 2) #""))))

(defn blocked? [seed point]
  (or (some neg? point)
      (odd? (ones-count (wall-formula point seed)))))

(defn taxicab-distance [[x1 y1] [x2 y2]]
 (+ (Math/abs (- x1 x2))
    (Math/abs (- y1 y2))))

(defn euclidean-distance [[x1 y1] [x2 y2]]
 (Math/sqrt (+ (Math/pow (- x1 x2) 2)
               (Math/pow (- y1 y2) 2))))

(defn neighbours [[x y]]
  (map (fn [[dx dy]] (vector (+ x dx) (+ y dy))) dirs))

(defn free-neighbours [seed [x y]]
  (remove (partial blocked? seed) (neighbours [x y])))

(defn get-or-max [map key]
  (get map key Integer/MAX_VALUE))

(defn find-path
  ([start goal seed distance-fn]
   (find-path goal (priority-map start 0) {start 0} seed distance-fn))
  ([goal queue costs seed distance-fn]
   (when-let [[current _] (peek queue)]
     (if (= current goal)
       (get costs current)
       (let [neighbours        (free-neighbours seed current)
             new-cost          (inc (get-or-max costs current))
             known-or-worse?   #(or (contains? costs %)
                                    (and (contains? queue %)
                                         (>= new-cost (get costs %))))
             to-visit          (doall (remove known-or-worse? neighbours))]
         (recur
           goal
           (into (pop queue) (doall (map #(vector % (+ new-cost (distance-fn % goal))) to-visit)))
           (reduce #(assoc %1 %2 new-cost) costs to-visit)
           seed
           distance-fn))))))

(defn reachable-locations [seed start steps visited]
  (if (= steps 0)
    (conj visited start)
    (conj
     (reduce
      #(clojure.set/union %1 (reachable-locations seed %2 (dec steps) (conj visited start)))
      #{}
      (clojure.set/difference (set (free-neighbours seed start)) visited))
     start)))

(defn process-a [input]
  (find-path [1 1] [31 39] input taxicab-distance))

(defn process-b [input]
  (count (reachable-locations input [1 1] 50 #{})))

(defn -main [& args]
  (let [result-a (process-a 1362)
        result-b (process-b 1362)]
    (println "First Part:" result-a)
    (println "Second Part:" result-b)))
