(ns day12.core
   (:require [clojure.string :as str])
   (:require [clojure.pprint :refer [pprint]])
   (:gen-class))

; http://rosettacode.org/wiki/Determine_if_a_string_is_numeric#Clojure
(defn numeric? [s]
  (if-let [s (seq s)]
    (let [s (if (= (first s) \-) (next s) s)
          s (drop-while #(Character/isDigit %) s)
          s (if (= (first s) \.) (next s) s)
          s (drop-while #(Character/isDigit %) s)]
      (empty? s))))

(def initial-state-a
  {:pc 0
   :reg { :a 0 :b 0 :c 0 :d 0}
   :code []})

(def initial-state-b
  {:pc 0
   :reg { :a 0 :b 0 :c 1 :d 0}
   :code []})

(defn inc-pc [state]
  (assoc state :pc (inc (get state :pc))))

(defn get-reg [state reg]
  (get-in state [:reg (keyword reg)]))

(defn set-reg [state reg val]
  (assoc-in state [:reg (keyword reg)] val))

(defn asb-cpy [state x y]
  (inc-pc (set-reg state y (if (numeric? x)
                            (read-string x)
                            (get-reg state x)))))

(defn asb-inc [state x]
  (inc-pc (set-reg state x (inc (get-reg state x)))))

(defn asb-dec [state x]
  (inc-pc (set-reg state x (dec (get-reg state x)))))

(defn asb-jnz [state x y]
  (if-not (= (get-reg state x) 0)
    (assoc state :pc (+ (read-string y) (get state :pc)))
    (inc-pc state)))

(defn exec-instruction [state fun params]
  (apply (partial fun state) params))

(defn set-code [state code]
  (assoc state :code code))

(defn execute-code [state]
  (if-let [instruction (get (:code state) (:pc state))]
    (let [asb-fun      (resolve (symbol (str "asb-" (first instruction))))
          asb-params   (rest instruction)]
      (recur (exec-instruction state asb-fun asb-params)))
    state))

(defn parse-input [raw]
  (apply vector (map #(str/split % #" ") (str/split-lines raw))))

(defn process-a [input]
  (get-in (execute-code (set-code initial-state-a input)) [:reg :a]))

(defn process-b [input]
  (get-in (execute-code (set-code initial-state-b input)) [:reg :a]))

(defn -main [& args]
  (ns day12.core)
  (let [input    (parse-input (slurp (first args)))
        result-a (process-a input)
        result-b (process-b input)]
    (println "First Part:" result-a)
    (println "Second Part:" result-b)))
