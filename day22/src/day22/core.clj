(ns day22.core
  (:gen-class)
  (:require [clojure.math.combinatorics :as cmb]
            [clojure.string :as str]))

(defn viable-pair? [a b]
  (and (not= (:pos a) (:pos b))
       (> (:used a) 0)
       (> (:avail b) (:used a))))

(defn parse-name [name]
  (let [[_ x y] (re-matches #".*node-x(\d+)-y(\d+)" name)]
    {:x (read-string x) :y (read-string y)}))

(defn parse-num [raw]
  (read-string (str/replace raw "T" "")))

(defn parse-perc [raw]
  (read-string (str/replace raw "%" "")))

(defn parse-row [[name size used avail useperc]]
  (hash-map :pos     (parse-name name)
            :size    (parse-num size)
            :used    (parse-num used)
            :avail   (parse-num avail)
            :useperc (parse-perc useperc)))

(defn parse-input [raw]
  (->> raw
       str/split-lines
       (drop 2)
       (map #(str/split % #"\s+"))
       (map parse-row)))

(defn char-for [el min]
  (cond
    (= (:useperc el) 0)        \_
    (> (:used el) min)         \#
    (= (:pos el) {:x 38 :y 0}) \G
    (= (:pos el) {:x 0 :y 0})  \S
    :otherwise                 \.))

(defn print-grid [input min]
  (doseq [row input]
    (doseq [elem row]
      (print "" (char-for elem min)))
    (print \newline)))

(defn to-grid [input]
  (mapv (fn [{row-pos :pos}]
          (filter #(= (get-in % [:pos :y])
                      (:y row-pos))
                  input))
        (->> input
             (filter #(= 0 (get-in % [:pos :x])))
             (sort-by #(get-in % [:pos :y])))))

(defn process-a [input]
  (->> input
       (#(cmb/cartesian-product % %))
       (filter #(viable-pair? (first %) (second %)))
       count))

(defn process-b [input]
  (println "Solve by hand:")
  (print-grid (to-grid input) (apply min (map :size input))))

(defn -main [& args]
  (let [input  (parse-input (slurp (first args)))
        result-a (process-a input)]
    (println "First Part:" result-a)
    (println "Second Part:")
    (process-b input)))
