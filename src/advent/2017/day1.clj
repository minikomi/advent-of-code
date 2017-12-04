(ns advent.2017.day1
  (:require [clojure.java.io :as io]))

(def input (slurp (io/resource "day1.txt")))

(defn rotate [n v]
  (into (subvec v n)
        (subvec v 0 n)))

(defn advent-1 [n input]
  (->> (map vector input (rotate n (vec input)))
       (filter #(apply = %))
       (map (comp read-string str first))
       (apply +)))

(defn solve-day-1 []
  (println "1-1:"
           (advent-1 1 input))
  ;; advent 1 - 2
  (println "1-2:"
           (advent-1 (/ (count input) 2) input)))
