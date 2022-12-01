(ns advent.2021.day1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent.util :refer [with-clipboard]]))

(def input (->> (io/resource "2022/day1-input1.txt")
                slurp))

(def test-input "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000")

(defn parse [input-str]
  (reduce (fn [acc line]
            (if (str/blank? line)
              (conj acc [])
              (update acc (dec (count acc)) conj (Integer/parseInt line))))
          [[]]
          (str/split-lines input-str)))

(defn solve1 [input-str]
  (->> input-str
       parse
       (map (fn [v] (apply + v)))
       (apply max)))

(defn solve2 [input-str]
  (->> input-str
       parse
       (map (fn [v] (apply + v)))
       (sort >)
       (take 3)
       (apply +)))

(comment
  (assert (= 24000 (solve1 test-input)))
  (with-clipboard (println (solve1 input)))
  (with-clipboard (println (solve2 input))))
