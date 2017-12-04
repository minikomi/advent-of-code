(ns advent.2017.day4
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def input
  (->> (io/resource "day4.txt")
       slurp
       s/split-lines
       (mapv #(s/split % #"\s+"))))

(count
 (filter #(= % (distinct %)) input))
