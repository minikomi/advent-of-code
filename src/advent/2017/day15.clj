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
  (= (bit-and a bottom-16-1)
     (bit-and b bottom-16-1)))

(defn count-matches [n as bs]
  (->> (map match as bs)
       (drop 1)
       (take n)
       (filter identity)
       count))

(defn judge [n a b]
  (let [as (iterate gen-a a)
        bs (iterate gen-b b)]
    (count-matches n as bs)))

(defn judge2 [n a b]
  (let [as (filter #(zero? (rem % 4)) (iterate gen-a a))
        bs (filter #(zero? (rem % 8)) (iterate gen-b b))]
    (count-matches n as bs)))

(defn judgeloop [stop a b]
  (loop [n 0 a a b b c 0]
    (if (= n stop) c
        (let [n* (inc n)
              a* (gen-a a)
              b* (gen-b b)
              c* (if (match a* b*) (inc c) c)]
          (recur n* a* b* c*)
          ))))

(comment (take 10 (iterate gen-a 65))

         (judge 40000000 65 8921)

         (judge2 5000000 277 349)

         (judgeloop 40000000 65 8921)

         (judgeloop 40000000 277 349)
         )
