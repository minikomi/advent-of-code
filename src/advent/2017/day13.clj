(ns advent.2017.day13
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [advent.util :as util]
            [clojure.set :as set]))

(def input-raw
  (s/trim (slurp (io/resource "2017/day13.txt"))))

(def get-states
  (memoize
   (fn [scan-range]
     (into (vec (range (dec scan-range)))
           (vec (range (dec scan-range) 0 -1))))))

(def test-input
  "0: 3
1: 2
4: 4
6: 4")

(defn parse-input [input]
  (into {}
        (mapv #(vec (map read-string (s/split % #": ")))
              (s/split-lines input))))

(parse-input test-input)

(defn solve1 [parsed delay]
  (let [max-slot (apply max (mapv first parsed))]
    (for [i (range (inc max-slot))
          :let [scan-range (get parsed i)]
          :when (and scan-range
                     (zero? (get (get-states scan-range)
                                 (mod (+ i delay)
                                      (- (* scan-range 2) 2)))))]
      (* i scan-range))))

(defn good-delay [parsed delay]
  (not-any?
   (fn [[n v]]
     (zero? (rem (+ n delay) (* 2 (dec v)))))
   parsed))

(defn solve2 [input]
  (let [parsed (vec (parse-input input))]
    (some #(when (good-delay parsed %) %)
          (range))))
