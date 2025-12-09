(ns day2
  (:require
   [clojure.string :as str]
   [instaparse.core :as insta]))

(def input1 "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,
1698522-1698528,446443-446449,38593856-38593862,565653-565659,
824824821-824824827,2121212118-2121212124")

(defn parse [s]
  (let [grammar "<ranges> = range+
                   range = number <'-'> number <','?> <'\n'?>
                   number = #'[0-9]+'"
        parser (insta/parser grammar)
        parse-tree (parser s)]
    (insta/transform
     {:range vector
      :number clojure.edn/read-string}
     parse-tree)))

(defn valid-id? [^Integer id]
  (let [^String s (str id)]
    (cond (odd? (count s)) true
          :else (let [mid-point (/ (count s) 2)]
                  (not= (subs s 0 mid-point)
                        (subs s mid-point))))))

(comment
  (parse input1)

  (for [d ["11" "010" "123" "1010" "446446" "1011"]]
    [d (valid-id? d)]))

(defn solve1 [input-str]
  (transduce
   (comp
    (mapcat (fn [[a b]] (range a (inc b))))
    (remove valid-id?))
   +
   (parse input-str)))

(comment
  (solve1 input1)
  (time (solve1 (slurp "./inputs/day2.txt"))))

(defn repeated-block? [^String s]
  (let [n (count s)]
    (let [pos (.indexOf (str s s) s 1)]
      (and (>= pos 1) (< pos n)))))

(comment (repeated-block? "ababab"))

(defn valid-id-2? [id]
  (let [s (str id)]
    (if
     (>= 10 id) true
     (not (repeated-block? s)))))

(comment
  (map (juxt identity valid-id-2?)
       [11
        22
        123123123 12121212 1112111211131112
        123124123]))

(defn solve2 [input-str]
  (transduce
   (comp
    (mapcat (fn [[a b]] (range a (inc b))))
    (remove valid-id-2?))
   +
   (parse input-str)))

(comment (solve2 input1))

(comment
  (time (solve2 (slurp "./inputs/day2.txt"))))

