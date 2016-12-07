(ns day7.core
   (:require [clojure.string :as str])
   (:gen-class))

(defn has-abba [block]
  (re-matches #".*(.)((?!\1).)\2\1.*" block))

(defn get-abas [block]
  (re-seq #"(.)(?!\1)(?=(.)\1)" block))

(defn has-bab [block, aba]
  (let [ [_ a b]     aba
         desired-bab (str b a b)]
    (str/includes? block desired-bab)))

(defn get-parts [block]
  (clojure.string/split block #"\[|\]"))

(defn get-hypernet-parts [block]
  (take-nth 2 (rest (get-parts block))))

(defn get-non-hypernet-parts [block]
  (take-nth 2 (get-parts block)))

(defn get-all-abas [parts]
  (apply concat (map get-abas parts)))

(defn has-matching-aba [abas block]
  (some (partial has-bab block) abas))

(defn supports-tls? [ip]
  (let [non-hypernet-parts (get-non-hypernet-parts ip)
        hypernet-parts     (get-hypernet-parts ip)
        has-no-abba        (complement has-abba)]
    (and (some   has-abba    non-hypernet-parts)
         (every? has-no-abba hypernet-parts))))

(defn supports-ssl? [ip]
  (let [non-hypernet-parts (get-non-hypernet-parts ip)
        hypernet-parts     (get-hypernet-parts ip)
        abas               (get-all-abas non-hypernet-parts)]
    (and abas
         (some (partial has-matching-aba abas) hypernet-parts))))

(defn parse-input [input]
  (str/split-lines input))

(defn process-a [input]
  (count (filter supports-tls? input)))

(defn process-b [input]
  (count (filter supports-ssl? input)))

(defn -main [& args]
 (let [input    (parse-input (slurp (first args)))
       result-a (process-a input)
       result-b (process-b input)]
   (println "First Part:" result-a)
   (println "Second Part:" result-b)))
