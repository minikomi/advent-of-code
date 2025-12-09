(ns advent.2022.day4
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def input (->> (io/resource "2022/day4-input1.txt")
                slurp))

(def test-input "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8")

(defn parse-input [s]
  (->> s
       (str/split-lines)
       (map str/trim)))

(defn parse-line [l]
  (vec (for [part (str/split l #",")]
     (vec (for [value (str/split part #"-")]
            (Integer/parseInt value))))))

(comment
  (parse-line "2-3,4-5")
  ;;
  )

(defn cover? [[[a b] [c d]]]
  (or (<= a c d b)
      (<= c a b d)))



(comment
  (cover? [[1 10][0 9]])
  ;;
  )

(defn solve [input]
  (->> (parse-input input)
       (map parse-line)
       (filter cover?)
       count
       ))

(comment
  (solve test-input)
  (solve input)
  )

;;  a ----- b
;;     c -------d
;;
;;  c-----d
;;     a-----b


(defn overlap? [[[a b][c d]]]
  (if (<= a c)
    (<= c b)
    (<= a d)))

(defn solve2 [input]
  (->> input
       parse-input
       (map parse-line)
       (filter overlap?)
       count))
(comment
  (solve2 input)
  ;;
  )
