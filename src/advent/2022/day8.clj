(ns advent.2022.day8
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn visible-from-left [row]
  (reduce
   #(if (or (empty? %)
            (< (second (last %))
               (second %2)))
      (conj % %2) %)
   []
   row))

(defn visible-left-right [row]
  {:left (visible-from-left row)
   :right (visible-from-left (reverse row))})

(def test-input "30373
25512
65332
33549
35390")

(defn parse-input [s]
  (let [lines (str/split-lines s)]
    (vec (for [y (range (count lines))]
           (vec (for [x (range (count (first lines)))
                      :let [c (get-in lines [y x])
                            i (Integer/parseInt (str c))]]
                  [[y x] i]))))))

(defn transpose-matrix [mtx]
  (apply mapv vector mtx))

(defn get-all-visible [grid]
  {:horiz (map visible-left-right grid)
   :vert (map visible-left-right (transpose-matrix grid))})

(defn solve [input]
  (let [grid (parse-input input)
        visible-raw (get-all-visible grid)
        visible-comb (into (:horiz visible-raw)
                           (:vert visible-raw))
        visible-all (map #(into (:left %) (:right %)) visible-comb)]
    (count (set (map first (reduce into visible-all))))))

(def input (->> (io/resource "2022/day8-input1.txt")
                slurp))

(comment

  (solve test-input)
  (solve input))
