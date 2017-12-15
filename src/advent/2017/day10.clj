(ns advent.2017.day10
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [advent.util :as util]))

(def input-raw
  (s/trim (slurp (io/resource "day10.txt"))))

(def input
  (map read-string (s/split input-raw #",")))

(def testlist [0 1 2 3 4])

(def testinput [3 4 1 5 1])

(defn rotate [v n]
  (into (subvec v (mod n (count v)))
        (subvec v 0 (mod n (count v)))))

(defn step [s [len skip]]
  (let [twisted (into (subvec s len)
                      (vec (reverse (subvec s 0 len))))]
    (rotate twisted skip)))

(def suffix [17 31 73 47 23])

(defn single-knot [steps [state initial-offset initial-skip]]
  (util/let-stop [skip-steps (map vector steps
                                  (map #(+ initial-skip %)
                                       (range)))
                  head-offset (apply + (flatten skip-steps))
                  knotted (reduce step
                                  state
                                  skip-steps)]
    [knotted
     (+ head-offset initial-offset)
     (+ initial-skip (count steps))]))

(defn solve1 [input]
  (let [[knotted head-offset _] (single-knot input [(vec (range 256)) 0 0])
        reverted (rotate knotted (- head-offset))]
    (* (reverted 0) (reverted 1))))

(defn knot [input]
  (util/let-stop [input-hex (into (mapv int (seq input))
                                  suffix)
                  [knotted head-offset _]
                  (nth (iterate (partial single-knot input-hex)
                                [(vec (range 256)) 0 0]) 64)
                  reverted (rotate knotted (- head-offset))]
    (map #(apply bit-xor %)
         (partition 16 reverted))))

(defn hash-format [xored]
  (apply str (map #(format "%02x" %) xored)))

(comment
  (solve1 input)
  (= "3efbe78a8d82f29979031a4aa0b16a9d" (hash-format (knot "1,2,3")))
  (= "63960835bcdc130f0b66d7ff4f6a5a8e" (hash-format (knot "1,2,4")))
  (println (knot input-raw))
  )
