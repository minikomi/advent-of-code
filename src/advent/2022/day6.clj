(ns advent.2022.day6
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set]))

(def input (->> (io/resource "2022/day7-input1.txt")
                slurp))

(def test-inputs ["mjqjpqmgbljsphdztnvjfqwrcgsmlb"
                  "bvwbjplbgvbhsrlpgdmjqwftvncz"
                  "nppdvjthqldpwncqszvftbrmjlhg"
                  "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
                  "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"])

(defn push-sliding [size v val] (conj (vec (take-last (dec size) v)) val))

(defn unique-char-seq [s]
  (reductions
   (fn [[idx seen recent] c]
     [(inc idx) (conj seen c) (push-sliding 4 recent c)])
   [0 #{} []]
   s))

(defn solve [s]
  (->> s
       (unique-char-seq)
       (drop-while
        (fn not-good-candidate [[_ seen recent]]
          (or
           (> 4 (count seen))
           (not= (count recent)
                 (count (set recent))))))
       ((comp first first))))

(comment
  (map solve test-inputs)
  (solve input)
  ;;
  )

(defn unique-char-seq2 [s]
  (reductions
   (fn [[idx seen recent] c]
     [(inc idx) (conj seen c) (push-sliding 14 recent c)])
   [0 #{} []]
   s))

(defn solve2 [s]
  (->> s
       (unique-char-seq2)
       (drop-while
        (fn not-good-candidate [[_ seen recent]]
          (or
           (> 14 (count seen))
           (not= (count recent)
                 (count (set recent))))))
       ((comp first first))))

(comment
  (map solve2 test-inputs)
  (solve2 input)
  ;;
  )
