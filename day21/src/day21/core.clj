(ns day21.core
  (:gen-class)
  (:require [clojure.string :as str]))

(defn sc-swap-pos [x y str]
  (let [a (nth str x)
        b (nth str y)]
    (-> (str/split str #"")
        (assoc x b)
        (assoc y a)
        str/join)))

(defn sc-swap-letter [a b str]
  (sc-swap-pos (str/index-of str a)
               (str/index-of str b)
               str))

(defn sc-rotate-left [rotate-by str]
  (->> str
       cycle
       (drop rotate-by)
       (take (count str))
       str/join))

(defn sc-rotate-right [rotate-by str]
  (sc-rotate-left (- (count str)
                     (mod rotate-by (count str)))
                  str))

(defn sc-reverse [from to in]
  (str (subs in 0 from)
       (-> in (subs from (inc to)) reverse str/join)
       (subs in (inc to))))

(defn remove-index [idx vector]
  (vec (concat (subvec vector 0 idx)
               (subvec vector (inc idx)))))

(defn set-index [idx val vector]
  (vec (concat (subvec vector 0 idx)
               [val]
               (subvec vector idx))))

(defn sc-move-pos [x y in]
  (->> (str/split in #"")
       vec
       (remove-index x)
       (set-index y (nth in x))
       str/join))

(defn sc-rotate-letter [letter str]
  (let [idx (str/index-of str letter)]
    (sc-rotate-right
     (+ 1 idx (if (>= idx 4) 1 0))
     str)))

(defn sc-rotate-letter-inverse [letter str]
  (let [idx         (str/index-of str letter)
        zero-or-odd (or (zero? idx) (odd? idx))]
    (sc-rotate-left (+ (quot idx 2)
                        (if zero-or-odd 1 5))
                    str)))

(def commands
  {#"swap position (\d) with position (\d)"      sc-swap-pos
   #"swap letter ([a-z]) with letter ([a-z])"    sc-swap-letter
   #"rotate left (\d) steps?"                    sc-rotate-left
   #"rotate right (\d) steps?"                   sc-rotate-right
   #"rotate based on position of letter ([a-z])" sc-rotate-letter
   #"reverse positions (\d) through (\d)"        sc-reverse
   #"move position (\d) to position (\d)"        sc-move-pos})

(def commands-inverse
  {#"swap position (\d) with position (\d)"      sc-swap-pos
   #"swap letter ([a-z]) with letter ([a-z])"    sc-swap-letter
   #"rotate left (\d) steps?"                    sc-rotate-right
   #"rotate right (\d) steps?"                   sc-rotate-left
   #"rotate based on position of letter ([a-z])" sc-rotate-letter-inverse
   #"reverse positions (\d) through (\d)"        sc-reverse
   #"move position (\d) to position (\d)"        (fn [x y str] (sc-move-pos y x str))})

(defn parse-arg [elem]
  (if (re-matches #"\d+" elem)
    (read-string elem)
    elem))

(defn str->commandfn [[re fun] line]
  (let [match (re-matches re line)
        args  (map parse-arg (rest match))]
    (apply partial (concat [fun] args))))

(defn parse-line [line command-set]
  (str->commandfn
   (first (filter #(re-matches (key %) line) command-set))
   line))

(defn parse-input [raw command-set]
  (mapv #(parse-line % command-set) (str/split-lines raw)))

(defn process-a [input unscrambled]
  (reduce #(apply %2 [%1]) unscrambled input))

(defn process-b [input scrambled]
  (reduce #(apply %2 [%1]) scrambled (reverse input)))

(defn -main [& args]
  (let [input-a  (parse-input (slurp (first args)) commands)
        input-b  (parse-input (slurp (first args)) commands-inverse)
        result-a (process-a input-a "abcdefgh")
        result-b (process-b input-b "fbgdceah")]
    (println "First Part:" result-a)
    (println "Second Part:" result-b)))
