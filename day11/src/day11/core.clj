(ns day11.core
   (:require [clojure.string :as str])
   (:require [clojure.math.combinatorics :refer [combinations cartesian-product]])
   (:require [clojure.set :refer [union difference]])
   (:require [clojure.pprint :refer [pprint]])
   (:require [clojure.data.priority-map :refer [priority-map]])
   (:gen-class))

(defn elem-map [& elems]
  (into (sorted-map) (map #(conj [%1] %2) elems (range 1 (inc (count elems))))))

(defn chip      [elem elems-map] (* -1 (elem elems-map)))
(defn generator [elem elems-map] (elem elems-map))

(defn target-state? [state]
  (and (= 3 (:elevator state))
       (every? empty? (subvec (:floors state) 0 3))))

(defn valid-floor? [floor]
  (or (or (empty? floor) (< (last floor) 0))
      (every? true? (map #(contains? floor (* -1 %)) (filter neg? floor)))))

(defn valid-state? [state]
  (every? valid-floor? (:floors state)))

(defn combinations-for-floor [floor]
  (map (partial apply sorted-set)
       (into (combinations (apply list floor) 2)
             (combinations (apply list floor) 1))))

(defn next-states [current-state]
  (let [{:keys [elevator floors]} current-state
        current-floor             (nth floors elevator)
        dirs                      (filter #(<= 0 % 3) (map (partial + elevator) [1 -1]))
        combinations              (combinations-for-floor current-floor)]
   (map (fn [[move dir]]
          (let [new-elevator dir
                new-floor    (nth floors new-elevator)]
           (assoc current-state
                  :elevator new-elevator
                  :floors   (assoc floors
                                   elevator      (difference current-floor move)
                                   new-elevator  (union      new-floor     move)))))
        (cartesian-product combinations dirs))))

(defn priority [state cost weight]
  (- cost (* (count (get-in state [:floors 3])) weight)))

(defn priority-a [state cost]
  (priority state cost 3))

(defn priority-b [state cost]
  (priority state cost 3))

(defn search
  ([initial-state priority-fn]
   (search (priority-map initial-state 0) {initial-state 0} priority-fn))
  ([queue costs priority-fn]
   (if (peek queue)
    (let [current                   (key (peek queue))
          queue                     (pop  queue)
          new-cost                  (inc (get costs current))
          {:keys [elevator floors]} current]
      (if-not (target-state? current)
       (let [next-states (filter valid-state? (next-states current))
             to-visit    (filter #(< new-cost (get costs % Integer/MAX_VALUE)) next-states)
             new-costs   (reduce #(assoc %1 %2 new-cost) costs to-visit)
             new-queue   (into queue (map #(vector % (priority-fn % new-cost)) to-visit))]
         (recur new-queue new-costs priority-fn))
       (get costs current)))
    costs)))

(defn process-a [input]
  (search input priority-a))

(defn process-b [input]
  (search input priority-b))

(def elements-a (elem-map :cobalt :curium :ruthenium :plutonium :promethium))
(def elements-b (elem-map :cobalt :curium :ruthenium :plutonium :promethium :elerium :dilithium))

(def initial-state-a
  (let [elems-map elements-a]
   {:elevator 0
    :floors [(sorted-set (chip :promethium elems-map)
                         (generator :promethium elems-map))
             (sorted-set (generator :cobalt elems-map)
                         (generator :curium elems-map)
                         (generator :ruthenium elems-map)
                         (generator :plutonium elems-map))
             (sorted-set (chip :cobalt elems-map)
                         (chip :curium elems-map)
                         (chip :ruthenium elems-map)
                         (chip :plutonium elems-map))
             #{}]}))

(def initial-state-b
  (let [elems-map elements-b]
   {:elevator 0
    :floors [(sorted-set (chip :promethium elems-map)
                         (generator :promethium elems-map)
                         (chip :elerium elems-map)
                         (generator :elerium elems-map)
                         (chip :dilithium elems-map)
                         (generator :dilithium elems-map))
             (sorted-set (generator :cobalt elems-map)
                         (generator :curium elems-map)
                         (generator :ruthenium elems-map)
                         (generator :plutonium elems-map))
             (sorted-set (chip :cobalt elems-map)
                         (chip :curium elems-map)
                         (chip :ruthenium elems-map)
                         (chip :plutonium elems-map))
             #{}]}))

(defn -main [& args]
 (println "First Part:" (process-a initial-state-a))
 (println "Second Part:" (process-b initial-state-b)))
