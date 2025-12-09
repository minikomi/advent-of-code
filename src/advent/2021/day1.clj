(ns advent.2021.day1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (->> (io/resource "2021/input1.txt")
                slurp
                str/split-lines
                (mapv #(Integer/parseInt %))))

(defn advent-1 [vs]
  (->> vs
       (partition 2 1)
       (filter #(apply < %))
       (count)))

(def test-input-1 [199
                   200
                   208
                   210
                   200
                   207
                   240
                   269
                   260
                   263])

(comment
  (advent-1 test-input-1) ; 7
  (advent-1 input))

(defn advent-2 [vs]
  (->> vs
       (partition 3 1)
       (map #(apply + %))
       (advent-1)))

(comment
  (advent-2 test-input-1) ; 5
  (advent-2 input))
