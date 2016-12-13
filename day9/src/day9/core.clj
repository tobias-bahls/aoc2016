(ns day9.core
   (:require [clojure.string :as str])
   (:gen-class))

(defn count-expanded-string [str, len-fn]
  (let [ [match before _ marker len times rest] (re-matches #"^([^\(]*)((\((\d+)x(\d+)\))(.*?))?" str)]
    (if (nil? marker)
      (count before)
      (let [times         (read-string times)
            len           (read-string len)
            bounded-len   (min len (count rest))
            marker-len    (len-fn bounded-len times rest)
            new-input     (subs str (+ (count marker) (count before) bounded-len))]
        (+ (count before)
           marker-len
           (count-expanded-string new-input len-fn))))))

(defn markerlen-a [len times rest]
   (* len times))

(defn markerlen-b [len times rest]
  (* times (count-expanded-string (subs rest 0 len) markerlen-b)))

(defn process-a [input]
  (count-expanded-string input markerlen-a))

(defn process-b [input] nil
 (count-expanded-string input markerlen-b))

(defn parse-input [input]
  (str/trim input))

(defn -main [& args]
 (let [input    (parse-input (slurp (first args)))
       result-a (process-a input)
       result-b (process-b input)]
   (println "First Part:" result-a)
   (println "Second Part:" result-b)))
