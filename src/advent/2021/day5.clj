(ns advent.2021.day5
  (:require [clojure.string :as str]
            [advent.util :refer [let-stop]]
            [advent.2021.day4 :refer [parse-int find-first all-coords]]
            [clojure.java.io :as io]))

(def test-input "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2")

(defn parse-row [r]
  (let [parts-s (str/split r #" -> ")
        parts (mapv #(str/split % #",") parts-s)]
    (mapv (fn [p] (mapv parse-int p)) parts)))
(defn parse-input [input] (mapv parse-row (str/split-lines input)))

(def parsed-test
  (->> test-input
       (str/split-lines)
       (mapv parse-row)))

(def parsed-input (->> "2021/input5.txt"
                       io/resource
                       slurp
                       (str/split-lines)
                       (mapv parse-row)
                ;;
                       ))
(defn rng [a b]
  (let [backwards? (> a b)
        end (if backwards? (dec b) (inc b))
        direction (if backwards? -1 1)]
    (range a end direction)))

(defn get-touched-points [[[x1 y1] [x2 y2]]]
  (cond
    (= x1 x2) (mapv (fn [n] [x1 n]) (rng y1 y2))
    (= y1 y2) (mapv (fn [n] [n y1]) (rng x1 x2))
    :else []))

(comment
  (get-touched-points [[1 3] [1 1]]))

(defn advent-1 [vents]
  (let-stop [all-touched (mapcat get-touched-points vents)
             dangerous (filter #(<= 2 (second %)) (frequencies all-touched))]
            (count dangerous)))

(comment (advent-1 parsed-test)
         (advent-1 parsed-input))

(defn get-touched-points-diag [[[x1 y1] [x2 y2]]]
  (cond
    (= x1 x2) (mapv (fn [n] [x1 n]) (rng y1 y2))
    (= y1 y2) (mapv (fn [n] [n y1]) (rng x1 x2))
    :else (mapv vector (rng x1 x2) (rng y1 y2))))

(defn advent-2 [vents]
  (let-stop [all-touched (mapcat get-touched-points-diag vents)
             dangerous (filter #(<= 2 (second %)) (frequencies all-touched))]
            (count dangerous)))

(comment (advent-2 parsed-input))
