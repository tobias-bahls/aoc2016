(ns day5.core
   (:require [clojure.string :as str]
             [digest :as digest])
   (:gen-class))

(defn num-filled [coll] (count (filter (complement nil?) coll)))

(defn char-to-int [chr]
  (try
    (Integer/parseInt (str chr))
    (catch Exception e 99999)))

(defn next-pwd-char-a [str, cur-pw]
  (let [prefix     (subs str 0 5)
        pw-index   (num-filled cur-pw)
        pw-char    (nth str 5)]
    (if (= "00000" prefix)
      [pw-char pw-index]
      nil)))

(defn next-pwd-char-b [str, cur-pw]
  (let [prefix     (subs str 0 5)
        pw-index   (char-to-int (nth str 5))
        pw-char    (nth str 6)]
    (if (and (= "00000" prefix)
             (< pw-index 8)
             (nil? (nth cur-pw pw-index "")))
      [pw-char pw-index]
      nil)))

(defn try-pw [cur-pw pw-try next-pwd-char-fn]
  (if-let [next-char (next-pwd-char-fn (digest/md5 pw-try) cur-pw)]
    (assoc cur-pw (last next-char) (first next-char))
    cur-pw))

(defn find-pw
  ([input next-pwd-char-fn]
   (find-pw input (vec (repeat 8 nil)) 0 next-pwd-char-fn))
  ([input cur-pw idx next-pwd-char-fn]
   (if (< (num-filled cur-pw) 8)
     (recur input
       (try-pw cur-pw (str input idx) next-pwd-char-fn)
       (inc idx)
       next-pwd-char-fn)
     cur-pw)))

(defn process-a [input]
  (str/join (find-pw input next-pwd-char-a)))

(defn process-b [input]
  (str/join (find-pw input next-pwd-char-b)))

(defn -main [& args]
 (println "Be patient...")
 (let [input    "cxdnnyjw"
       result-a (process-a input)
       result-b (process-b input)]
   (println "First Part:" result-a)
   (println "Second Part:" result-b)))
