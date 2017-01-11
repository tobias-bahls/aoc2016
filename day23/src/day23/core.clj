(ns day23.core
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
   :reg { :a 7 :b 0 :c 0 :d 0}
   :code []})

(def initial-state-b
  {:pc 0
   :reg { :a 12 :b 0 :c 0 :d 0}
   :code []})

(defn reg-value-or-number [state x]
  (if (numeric? x) (read-string x) (get-reg state x)))

(defn inc-pc [state]
  (assoc state :pc (inc (get state :pc))))

(defn get-reg [state reg]
  (get-in state [:reg (keyword reg)]))

(defn set-reg [state reg val]
  (assoc-in state [:reg (keyword reg)] val))

(defn asb-cpy [state x y]
  (if (numeric? y)
    state
    (inc-pc (set-reg state y (reg-value-or-number state x)))))

(defn asb-inc [state x]
  (inc-pc (set-reg state x (inc (get-reg state x)))))

(defn asb-dec [state x]
  (inc-pc (set-reg state x (dec (get-reg state x)))))

(defn asb-jnz [state x y]
  (if-not (= (reg-value-or-number state x) 0)
    (assoc state :pc (+ (reg-value-or-number state y) (get state :pc)))
    (inc-pc state)))

(defn transform-instruction [[ins & args]]
  (cond
    (= 1 (count args)) (if (= ins "inc") "dec" "inc")
    (= 2 (count args)) (if (= ins "jnz") "cpy" "jnz")))

(defn asb-tgl [state x]
  (let [steps (reg-value-or-number state x)]
    (if (= 0 steps)
      (inc-pc state)
      (let [target      (+ (:pc state) steps)
            instruction (get-in state [:code target] nil)]
        (if instruction
          (inc-pc (assoc-in state [:code target 0] (transform-instruction instruction)))
          (inc-pc state))))))

(defn asb-mul [state x y z]
  (inc-pc (-> state
              (set-reg x (+ (get-reg state x) (* (get-reg state y) (get-reg state z))))
              (set-reg z 0)
              (set-reg "c" 0))))

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
  (in-ns 'day23.core)
  (let [input-a  (parse-input (slurp (first args)))
        input-b  (parse-input (slurp (second args)))
        result-a (process-a input-a)
        result-b (process-b input-b)]
    (println "First Part:" result-a)
    (println "Second Part:" result-b)))
