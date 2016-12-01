(ns day1.core
   (:require [clojure.string :as str])
   (:gen-class))

(def initial-state {:facing 0 :x 0 :y 0 :visited []})
(def relative-offset { 0 {:x  0 :y  1}
                       1 {:x  1 :y  0}
                       2 {:x  0 :y -1}
                       3 {:x -1 :y 0}})

(defn coords [state]
  {:x (:x state) :y (:y state)})

(defn turn [dir state]
  (assoc state :facing (case dir
                        \L (mod (dec (:facing state)) 4)
                        \R (mod (inc (:facing state)) 4))))

(defn distance [state]
  (+ (Math/abs (:x state)) (Math/abs (:y state))))

(defn record-visit [state]
  (merge-with conj state {:visited (coords state)}))

(defn walk [blocks state]
  (if (> blocks 0)
   (let [state-recorded (record-visit state)
         new-state (merge-with + state-recorded (get relative-offset (:facing state)))]
      (recur (dec blocks) new-state))
   state))

(defn step [state [dir blocks]]
  (walk blocks (turn dir state)))

(defn parse-instruction [[dir & rest]]
  [dir (-> rest str/join str/trim Integer/parseInt)])

(defn parse-input [input]
  (map parse-instruction (str/split input #", ")))

(defn process-grid-a [state [current-instruction & rest]]
  (if current-instruction
   (recur (step state current-instruction) rest)
   state))

(defn first-dup [seq]
  (let [all-dups (set (for [[id freq] (frequencies seq) :when (> freq 1)] id))]
    (first (filter #(contains? all-dups %) seq))))

(defn process-grid-b [state input]
  (first-dup (:visited (process-grid-a state input))))

(defn -main [& args]
  (let [input (parse-input (slurp (first args)))
        result-a (process-grid-a initial-state input)
        result-b (process-grid-b initial-state input)]
    (println "First Part:" (distance result-a))
    (println "Second Part:" (distance result-b))))
