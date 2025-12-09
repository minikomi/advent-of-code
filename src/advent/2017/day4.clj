(ns advent.2017.day4
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def input
  (->> (io/resource "2017/day4.txt")
       slurp
       s/split-lines
       (mapv #(s/split % #"\s+"))))

(defn has-anagrams? [row]
  (apply distinct? (map frequencies row)))

(comment
  (count
   (filter #(apply distinct? %) input))
  (count
   (filter has-anagrams? input)))
