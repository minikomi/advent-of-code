(ns day3
  (:require
   [clojure.string :as str]
   [criterium.core :as c]
   [instaparse.core :as insta]))

(def input1 "987654321111111
811111111111119
234234234234278
818181911112111")

(defn parse [s]
  (let [grammar "
banks = bank (whitespace bank)*
bank = digit+
<whitespace> = <#'\\s+'>
digit = #'[0-9]'"
        parser (insta/parser grammar)
        parse-tree (parser s)]
    (insta/transform
     {:banks vector
      :bank vector
      :digit clojure.edn/read-string}
     parse-tree)))

(defn find-max-joltage [bank n]
  (let [total (count bank)
        can-remove (- total n)]
    (loop [result []
           pos 0
           removals-left can-remove]
      (if (= pos total)
        (take n result)
        (let [digit (nth bank pos)]
          (if (and (pos? removals-left)
                   (seq result)
                   (> digit (peek result)))
            (recur (pop result) pos (dec removals-left))
            (recur (conj result digit) (inc pos) removals-left)))))))

(defn solve1 [input-str]
  (transduce
   (comp
    (map #(find-max-joltage % 2))
    (map (fn [[a b]] (+ (* 10 a) b))))
   + 0
   (parse input-str)))

(map (juxt identity #(find-max-joltage % 2)) (parse input1))
(solve1 input1)

(comment
  (solve1 (str/trim (slurp "./inputs/day3.txt"))))

(defn solve2 [inputs]
  (transduce
   (comp
    (map #(find-max-joltage % 12))
    (map (fn [ns] (reduce (fn ^long [^long v ^long n] (+ (* 10 v) n)) 0 ns))))
   + 0
   inputs))

(def ins (parse (str/trim (slurp "./inputs/day3.txt"))))

(comment (c/quick-bench (solve2 ins))

; Evaluation count : 528 in 6 samples of 88 calls.
;              Execution time mean : 1.181231 ms
;     Execution time std-deviation : 36.333780 µs
;    Execution time lower quantile : 1.145364 ms ( 2.5%)
;    Execution time upper quantile : 1.228786 ms (97.5%)
;                    Overhead used : 1.655558 ns
         )
