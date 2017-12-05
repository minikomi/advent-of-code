(ns advent.2017.day5
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def input
  (->> (io/resource "day5.txt")
       slurp
       s/split-lines
       (mapv read-string)))

(defn solve [input]
  (loop [i input idx 0 c 0]
    (if (or (> 0 idx)
            (<= (count input) idx)) c
        (recur (update i idx inc)
               (+ idx (get i idx))
               (inc c)))))

(defn solve2 [input]
  (loop [i input idx 0 c 0]
    (if (or (> 0 idx)
            (<= (count input) idx)) c
        (let [v (get i idx)]
          (recur (if (<= 3 v)
                   (update i idx dec)
                   (update i idx inc))
                 (+ idx (get i idx))
                 (inc c))))))


(defn solve2-transient [input]
  (loop [i (transient input) idx 0 c 0]
    (if (or (> 0 idx)
            (<= (count input) idx)) c
        (let [v (get i idx)]
          (recur (if (<= 3 v)
                   (assoc! i idx (dec (get i idx)))
                   (assoc! i idx (inc (get i idx))))
                 (+ idx v)
                 (inc c))))))

(comment (solve input)
         (solve2 [0 3 0 1 -3])
         (solve2 input)
         (solve2-transient input))
