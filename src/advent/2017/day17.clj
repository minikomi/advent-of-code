(ns advent.2017.day17
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [advent.util :as util]))

(defn one-spin [spin [vs pos] n]
  (let [new-pos (mod (+ pos spin) n)]
    [(->
      (subvec vs 0 (inc new-pos))
      (into [n])
      (into (subvec vs (inc new-pos))))
     (inc new-pos)]))

(defn solve1 []
  (let [[vs pos]
        (reduce #(one-spin 301 % %2)
                [[0] 0]
                (map inc (range 2017)))]
    (get vs (inc (mod pos 2017)))))

(defn solve2 [step times]
  (loop [pos 0 after-zero nil n 1]
    (if (> n times) after-zero
        (let [new-pos (inc (mod (+ pos step) n))]
          (recur
           new-pos
           (if (= 1 new-pos) n after-zero)
           (inc n))))))

(comment (solve1)
         (solve2 301 50000000))
