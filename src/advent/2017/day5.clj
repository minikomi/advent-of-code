(ns advent.2017.day5
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def input
  (->> (io/resource "day5.txt")
       slurp
       s/split-lines
       (mapv #(Integer/parseInt %))))

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
                 (+ idx v)
                 (inc c))))))

(defn solve2-transient [input]
  (loop [i (transient input) idx 0 c 0]
    (if (or (> 0 idx)
            (<= (count input) idx)) c
        (let [^long v (get i idx)]
          (recur (if (<= 3 v)
                   (assoc! i idx (dec v))
                   (assoc! i idx (inc v)))
                 (+ idx v)
                 (inc c))))))


(defn solve2-int-array [input]
  (let [i (int-array input)
        len (count i)]
    (loop [idx (int 0) c (int 0)]
      (if (or (> 0 idx) (<= len idx)) c
          (let [v (aget i idx)]
            (if (<= 3 v)
              (aset i idx (dec v))
              (aset i idx (inc v)))
            (recur (+ idx v)
                   (inc c)))))))

(comment (solve input)
         (solve2-transient [0 3 0 1 -3])
         (solve2 input)
         (solve2-transient input)
         (solve2-int-array int-arr-input)
         (bork-int-arr input)
         )
