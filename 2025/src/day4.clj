(ns day4
  (:require
   [clojure.string :as str]
   [criterium.core :as c]
   [instaparse.core :as insta]))

(def input1  "..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.")

(defn create-alive-set [input-str]
  (let [mtx (mapv vec (vec (str/split-lines input-str)))]
    (into #{}
          (for [y (range (count mtx))
                x (range (count (first mtx)))
                :when (= (get-in mtx [y x]) \@)]
            [y x]))))

(comment (create-alive-set input1))

(defn check-surrounding-count [alive-set [y x]]
  (let [deltas [[-1 -1] [-1 0] [-1 1]
                [0 -1]         [0 1]
                [1 -1] [1 0] [1 1]]]
    (transduce
     (map (fn [[dy dx]] [(+ y dy) (+ x dx)]))
     (completing (fn [acc coord] (if (contains? alive-set coord) (inc acc) acc)))
     0
     deltas)))

(defn solve1 [in-str]
  (let [alive-set (create-alive-set in-str)
        ok (volatile! 0)]
    (doseq [cell alive-set
            :when (> 4 (check-surrounding-count alive-set cell))]
      (vswap! ok inc))
    @ok))

(comment (solve1 (slurp "./inputs/day4.txt")))

(defn find-removable [alive-set]
  (filter
   #(> 4 (check-surrounding-count alive-set %))
   alive-set))

(defn solve2 [in-str]
  (let [alive-set (volatile! (create-alive-set in-str))
        generations (repeatedly
                     (fn []
                       (let [removable (find-removable @alive-set)]
                         (vswap! alive-set (fn [as] (reduce disj as (set removable))))
                         (count removable))))]
    (transduce (take-while pos?) + 0 generations)))

(comment (solve2 input1))

(comment (let [inp (slurp "./inputs/day4.txt")]
           (c/quick-bench (solve2 inp))))

