(ns day10.core
   (:require [clojure.string :as str])
   (:gen-class))

(def network (atom []))

(defn reset-network []
  (reset! network []))

(defn add-to-network [elem]
  (let [atom-elem (atom elem)]
    (do (swap! network conj atom-elem) atom-elem)))

(defn create-output [number]
 (add-to-network { :type :out :number number :val nil}))

(defn get-network-elem [type num]
  (first (filter #(and (= num  (:number (deref %1)))
                       (= type (:type   (deref %1))))
                 @network)))

(defn find-bot-with-vals [low high]
  (first (filter #(and (= :bot  (:type   (deref %1)))
                       (= low   (min (:val1 (deref %1)) (:val2 (deref %1))))
                       (= high  (max (:val1 (deref %1)) (:val2 (deref %1)))))
                 @network)))

(defn get-or-create [type num]
  (if-let [elem (get-network-elem type num)]
    elem
    (add-to-network { :type type :number num :val1 nil :val2 nil})))

(defmulti put-val (fn [ [type id] val] type))

(defmethod put-val :bot [[_ id] val]
  (let [bot   (get-or-create :bot id)
        val1  (:val1 (deref bot))
        val2  (:val2 (deref bot))]
    (if val1
      (swap! bot assoc :val2 val)
      (swap! bot assoc :val1 val))))

(defmethod put-val :out [[_ id] val]
  (let [out   (get-or-create :out id)]
    (swap! out assoc :val val)))

(defn setup-bot [bot low high]
  (add-watch bot :watcher
             (fn [key atom old-state new-state]
               (let [val1 (:val1 new-state)
                     val2 (:val2 new-state)]
                (if (and val1 val2)
                  (if (>= val1 val2)
                    (do (put-val high val1) (put-val low val2))
                    (do (put-val high val2) (put-val low val1))))))))
(defn parse-target[target]
  (if (= target "bot") :bot :out))

(defn parse-bot-line [line]
  (let [ [_ bot-num low-target low-num high-target high-num] (re-matches #"bot (\d+) gives low to (bot|output) (\d+) and high to (bot|output) (\d+)" line)
         low-target  (parse-target low-target)
         high-target (parse-target high-target)
         low-num     (read-string  low-num)
         high-num    (read-string  high-num)]
    {:bot (read-string bot-num) :low [low-target low-num] :high [high-target high-num]}))

(defn parse-value-line [line]
  (let [ [_ value bot] (re-matches #"value (\d+) goes to bot (\d+)" line)
         value  (read-string value)
         bot    (read-string bot)]
    {:bot bot :value value}))

(defn parse-input [input]
  (reduce
   (fn [instructions line]
     (cond
      (str/starts-with? line "value") (merge-with conj instructions {:inputs (parse-value-line line)})
      (str/starts-with? line "bot")   (merge-with conj instructions {:links  (parse-bot-line line)})))
   {:inputs [] :links []} (str/split-lines input)))

(defn setup-network [links]
  (doseq [link links]
    (setup-bot (get-or-create :bot (:bot link)) (:low link) (:high link))))

(defn provide-inputs [inputs]
  (doseq [input inputs]
    (put-val [:bot (:bot input)] (:value input))))

(defn init-network [input]
  (reset-network)
  (setup-network  (:links  input))
  (provide-inputs (:inputs input)))

(defn process-a [input]
  (init-network input)
  (:number (deref (find-bot-with-vals 17 61))))

(defn process-b [input] nil
  (init-network input)
  (* (:val (deref (get-network-elem :out 0)))
     (:val (deref (get-network-elem :out 1)))
     (:val (deref (get-network-elem :out 2)))))

(defn -main [& args]
 (let [input    (parse-input (slurp (first args)))
       result-a (process-a input)
       result-b (process-b input)]
   (println "First Part:" result-a)
   (println "Second Part:" result-b)))
