(ns advent.2022.day3
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [advent.util :refer [with-clipboard]]
            [clojure.set :as set]))

(def input (->> (io/resource "2022/day3-input1.txt")
                slurp))

(def test-input "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw")

(defn split-string-in-half [s]
  (let [len (count s)
        half-len (quot len 2)]
    [(subs s 0 half-len) (subs s half-len len)]))

(defn parse-input [s]
  (->> s
       (str/split-lines)
       (map str/trim)))

(defn find-common [strings]
  (first (apply set/intersection (map set strings))))

(comment
  (find-common (first (parse-input test-input))))

(defn score-char
  "Lowercase item types a through z have priorities 1 through 26.
    Uppercase item types A through Z have priorities 27 through 52."
  [c]
  (let [b (int c)
        offset (if (<= 97 b) 96 38)]
    (- b offset)))

(defn solve1 [s]
  (->> (parse-input s)
       (map split-string-in-half)
       (map find-common)
       (map score-char)
       (apply +)))

(comment
  (map score-char "abdefg")
  (map score-char "ABCDEFG")

  (assert
   (= 157 (solve1 test-input)))

  (solve1 input))

(defn solve2 [s]
  (->> (parse-input s)
       (partition 3)
       (map find-common)
       (map score-char)
       (apply +)))

(comment
  (solve2 test-input)
  (solve2 input))
