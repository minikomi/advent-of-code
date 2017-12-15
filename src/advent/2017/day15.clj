(ns advent.2017.day15
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [advent.util :as util]))


(def input [277 349])

(defn gen-a [v]
  (rem (* v 16807)
       2147483647))

(defn gen-b [v]
  (rem (* v 48271)
       2147483647))

(def bottom-16-1 (dec (* 16 16 16 16)))

(defn match [a b]
  (= bottom-16-1
     (bit-and bottom-16-1
              (bit-xor a (bit-not b)))))

(defn count-matches [n as bs]
  (reduce #(if %2 (inc %) %)
          0
          (take n (map match (drop 1 as) (drop 1 bs)))))

(defn judge [n a b]
  (let [as (iterate gen-a a)
        bs (iterate gen-b b)]
    (count-matches n as bs)))

(defn judge2 [n a b]
  (let [as (filter #(zero? (rem % 4)) (iterate gen-a a))
        bs (filter #(zero? (rem % 8)) (iterate gen-b b))]
    (count-matches n as bs)))


(take 10 (iterate gen-a 65))

(judge 40000000 65 8921)

(judge2 5000000 277 349)
