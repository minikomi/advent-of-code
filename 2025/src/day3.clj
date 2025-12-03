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
  (loop [n n
         befores (vec (reverse (map-indexed vector bank)))
         afters []
         collect []]
    (cond (zero? n) (map second (sort-by first collect))
          (empty? befores)
          (recur n (peek afters) (pop afters) collect)
          :else
          (let [[idx max1] (first (sort
                                   (fn [[i1 [_ v1]]
                                        [i2 [_ v2]]]
                                     (if (= v1 v2)
                                       (> i1 i2)
                                       (>= v1 v2)))
                                   (map-indexed vector befores)))
                bef (subvec befores 0 idx)
                aft (subvec befores (inc idx))]
            (recur (dec n)
                   bef
                   (conj afters aft)
                   (conj collect max1))))))

(comment
  (map (juxt identity #(find-max-joltage % 12)) (parse input1)))

(defn solve1 [input-str]
  (transduce
   (comp
    (map #(find-max-joltage % 2))
    (map (fn [[a b]] (+ (* 10 a) b))))
   + 0
   (parse input-str)))

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

(solve2 (str/trim (slurp "./inputs/day3.txt")))
