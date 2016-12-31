(ns day14.core
   (:require [while-let.core :refer [while-let]])
   (:require [clojure.string :as str])
   (:require [digest :as digest])
   (:require [clojure.pprint :refer [pprint]])
   (:require [clojure.core.async :as a :refer
              [>! <! >!! <!! go chan buffer close! thread alts! alts!! timeout]])
   (:gen-class))

(defn process-value [value stretch]
  (let [hash        (reduce (fn [hash, _] (digest/md5 hash)) (digest/md5 value) (range 0 stretch))
        triplet     (re-find #"([a-z0-9])\1{2}" hash)
        quintuplets (set (map #(second %) (re-seq #"([a-z0-9])\1{4}" hash)))]
    {:triplet      (if (nil? triplet) nil (second triplet))
     :quintuplets  (if (nil? quintuplets) #{} quintuplets)}))

(defn make-worker [in store seed stretch]
  (go (while-let
       [req (<!! in)]
       (let [new-values (reduce #(assoc %1 %2 (process-value (str seed %2) stretch)) {} req)]
         (swap! store #(merge % new-values))))))

(def step-size 1000)
(defn compute-values
  ([seed start max stretch]
   (let [channel  (chan 1)
         data     (atom {})]
    (make-worker channel data seed stretch)
    (make-worker channel data seed stretch)
    (make-worker channel data seed stretch)
    (make-worker channel data seed stretch)
    (doseq [bounds (partition 2 1 (take (+ (/ max step-size) 4) (iterate (partial + step-size) start)))]
          (>!! channel (apply range bounds)))
    (close! channel)
    (Thread/sleep 1000)
    data)))

(defn is-key? [[idx chr] values]
  (some #(contains? (:quintuplets (get values %)) (:triplet chr)) (range (inc idx) (+ idx 1001))))

(defn find-keys
  ([seed start max]
   (find-keys seed start max 0))
  ([seed start max stretch]
   (let [values        @(compute-values seed start (+ max 1000) stretch)
         with-triplets (remove #(nil? (:triplet (second %))) values)
         keys          (map key (sort-by key (filter #(is-key? % values) with-triplets)))]
    keys)))

(defn process-a [input]
  (nth (find-keys input 0 36000) 63))

(defn process-b [input]
  (nth (find-keys input 0 36000 2016) 63))

(defn -main [& args]
  (let [result-a (process-a "jlmsuwbz")
        result-b (process-b "jlmsuwbz")]
    (println "First Part:" result-a)
    (println "Second Part:" result-b)))
