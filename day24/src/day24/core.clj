(ns day24.core
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as cmb])
   (:require [clojure.pprint :refer [pprint]])
   (:gen-class))

(def dirs [[1 0] [-1 0] [0 1] [0 -1]])

(defn is-blocked? [maze [x y]]
  (if (or (< x 0) (< y 0) (> x (count (first maze))) (> y (count maze)))
    true
    (let [tile (get-in maze [y x])]
      (if (integer? tile)
        false
        tile))))

(defn get-neighbours [maze current]
  (remove #(is-blocked? maze %) (mapv #(mapv + current %) dirs)))

(defn bfs
  ([start end maze]
   (bfs start end maze [start] {start 0}))
  ([start end maze queue distances]
   (if (= end (first queue))
     (get distances end)
     (let [current         (first queue)
           queue           (apply vector (rest queue))
           current-dist    (get distances current)
           neighbours      (remove #(contains? distances %) (get-neighbours maze current))
           neighbour-dists (reduce #(assoc %1 %2 (+ current-dist 1)) {} neighbours)]
       (recur start end maze (apply conj queue neighbours) (merge distances neighbour-dists))))))

(defn distances-between-pois [maze]
  (apply hash-map
         (apply concat
                (let [pairs (cmb/cartesian-product (:pois maze) (:pois maze))]
                  (map (fn [[[p1 start] [p2 end]]]
                         [[p1 p2] (bfs start end (:map maze))]) pairs)))))

(defn shortest-path-in-perms [distances perms]
  (->> perms
       (map (partial partition 2 1))
       (map (partial map #(get distances (apply vector %))))
       (map (partial reduce +))
       (apply min)))

(defn shortest-path-from-zero [points distances]
  (->> points
      rest
      cmb/permutations
      (map (partial into [0]))
      (shortest-path-in-perms distances)))

(defn shortest-path-from-zero-to-zero [points distances]
  (->> points
       rest
       cmb/permutations
       (map #(into [0] (conj (apply vector %) 0)))
       (shortest-path-in-perms distances)))

;; Parsing
(defn parse-char [chr]
  (cond
    (= chr \#) true
    (= chr \.) false
    :otherwise (read-string (str chr))))

(defn parse-line [line]
  (mapv parse-char line))

(defn parse-map [raw]
  (mapv parse-line (str/split-lines raw)))

(defn find-pois [map]
  (apply hash-map
         (apply concat
                (for [[y row] (map-indexed list map)
                      [x cell] (map-indexed list row)
                      :when (integer? cell)]
                  [cell [x y]]))))

(defn parse-input [raw]
  (let [map (parse-map raw)]
    {:map  map
     :pois (find-pois map)}))

;; Main
(defn process-a [input]
  (shortest-path-from-zero (map key (:pois input))
                           (distances-between-pois input)))

(defn process-b [input]
  (shortest-path-from-zero-to-zero (map key (:pois input))
                                   (distances-between-pois input)))

(defn -main [& args]
  (let [input  (parse-input (slurp (first args)))
        result-a (process-a input)
        result-b (process-b input)]
    (println "First Part:" result-a)
    (println "Second Part:" result-b)))
