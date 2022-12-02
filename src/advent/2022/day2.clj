(ns advent.2022.day2
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [advent.util :refer [with-clipboard]]))

(def input (->> (io/resource "2022/day2-input1.txt")
                slurp))

(def test-input "A Y
B X
C Z")

(def shape-scores {:rock 1
                   :paper 2
                   :scissors 3})

(defn parse-shape [c]
  (case c
    "A" :rock
    "B" :paper
    "C" :scissors
    "X" :rock
    "Y" :paper
    "Z" :scissors))

(defn parse-strategy [c]
  (case c "X" :lose
        "Y" :draw
        "Z" :win))

(defn get-lose [shape]
  (case shape
    :rock :scissors
    :paper :rock
    :scissors :paper))

(defn get-win [shape]
  (case shape
    :rock :paper
    :paper :scissors
    :scissors :rock))

(defn calc-score [[a b]]
  (+
   (shape-scores b)
   (cond
     (= a (get-win b)) 0
     (= a (get-lose b)) 6
     :else 3)))

(defn parse-input [input]
  (for [line (str/split-lines input)]
    (let [[shape1 shape2] (str/split line #" ")]
      [(parse-shape shape1) (parse-shape shape2)])))

(comment
  (apply + (map calc-score (parse-input test-input)))

  (apply + (map calc-score (parse-input input))))

(defn parse-2 [input]
  (for [line (str/split-lines input)]
    (let [[shape1 stg] (str/split line #" ")]
      [(parse-shape shape1) (parse-strategy stg)])))

(defn chose-strategy-shape [[shape1 strat]]
  (case strat
    :win (get-win shape1)
    :lose (get-lose shape1)
    :draw shape1))

(comment
  (with-clipboard
    (println (->> input
                  parse-2
                  (map (fn [pair] [(first pair) (chose-strategy-shape pair)]))
                  (map calc-score)
                  (apply +))))
;;
  )
