(ns day17.core
  (:gen-class)
  (:require [clojure.string :as str]
            [digest :as digest]))

(def dir-names [\U \D \L \R])
(def dir-coords [ [0 -1] [0 1] [-1 0] [1 0] ])

(defn open-char? [char]
  (#{\b \c \d \e \f} char))

(defn step-hash [seed steps]
  (digest/md5 (str seed (str/join steps))))

(defn open-doors [seed steps]
  (let [memoized-hash (memoize step-hash)]
    (->> dir-names
         (map vector (take 4 (memoized-hash seed steps)))
         (filter #(-> % first open-char? boolean))
         (map second)
         set)))

(defn in-bounds? [[x y]]
  (and (or (zero? x) (pos? x)) (< x 4)
       (or (zero? y) (pos? y)) (< y 4)))

(defn neighbours [[x y]]
  (apply hash-map
         (mapcat (fn [[dx dy] dir]
                   (vector dir (vector (+ dx x) (+ dy y))))
                 dir-coords
                 dir-names)))

(defn open-neighbours [pos seed steps-taken]
  (let [neighbours (neighbours pos)
        doors      (open-doors seed steps-taken)]
    (->> neighbours
         (filter #(in-bounds? (second %)))
         (filter #(doors (first %))))))

(defn get-or-max [map key]
  (get map key Integer/MAX_VALUE))

(defn queue-entry [steps-taken [dir pos]]
  (vector pos
          (conj steps-taken dir)))

(defn all-paths
  ([start end seed]
   (all-paths start end seed '([[0 0] []]) []))
  ([start end seed queue results]
   (if-let [[pos steps] (peek queue)]
     (let [is-end-node?     #(= (first %) end)
           neighbour-nodes  (map (partial queue-entry steps)
                                 (open-neighbours pos seed steps))
           destination-nodes (filter is-end-node? neighbour-nodes)
           to-visit          (remove is-end-node? neighbour-nodes)]
       (recur start
              end
              seed
              (apply conj (pop queue)  to-visit)
              (apply conj results      destination-nodes)))
     results)))

(defn sorted-paths [start end seed]
  (sort-by #(count (second %)) (all-paths start end seed)))

(defn shortest-path [start end seed]
  (first (sorted-paths start end seed)))

(defn longest-path [start end seed]
  (last (sorted-paths start end seed)))

(defn process-a [input]
  (str/join (second (shortest-path [0 0] [3 3] input))))

(defn process-b [input]
  (count (second (longest-path [0 0] [3 3] input))))

(defn -main [& args]
  (let [result-a (process-a "njfxhljp")
        result-b (process-b "njfxhljp")]
    (println "First Part:" result-a)
    (println "Second Part:" result-b)))
