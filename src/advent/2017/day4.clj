(ns advent.2017.day4
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def input
  (->> (io/resource "day4.txt")
       slurp
       s/split-lines
       (mapv #(s/split % #"\s+"))))

(defn has-anagrams? [row]
  (when (not=
         (count (map sort row))
         (count (distinct (map sort row))))
    row))

(comment
  (count
   (filter #(= (count %) (count (distinct %))) input))
  (count
   (filter #(not (has-anagrams? %)) input)))
