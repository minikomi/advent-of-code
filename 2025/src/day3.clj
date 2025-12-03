(ns day3
  (:require
   [clojure.string :as str]
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

(defn solve2 [input-str]
  (transduce
   (comp
    (map #(find-max-joltage % 12))
    (map (fn [ns] (clojure.edn/read-string (apply str ns)))))
   + 0
   (parse input-str)))

(time (solve2 (str/trim (slurp "./inputs/day3.txt"))))
