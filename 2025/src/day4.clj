(ns day4
  (:require
   [clojure.string :as str]
   [criterium.core :as c]
   [dom-top.core :refer [loopr]]
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
  (let [mtx (mapv vec (str/split-lines input-str))]
    (loopr [alive #{}]
           [y (range (count mtx))
            x (range (count (first mtx)))]
           (recur (if (= (get-in mtx [y x]) \@)
                    (conj alive [y x])
                    alive)))))

(comment (create-alive-set input1))

(defn check-surrounding-count [alive-set [y x]]
  (loopr [count 0]
         [dy [-1 0 1]
          dx [-1 0 1]]
         (recur
          (if (and
               (not (and (= dy 0) (= dx 0)))
               (contains? alive-set [(+ y dy) (+ x dx)]))
            (inc count)
            count))))

(comment
  (check-surrounding-count
   (create-alive-set input1)
   [1 0]))

(defn solve1 [in-str]
  (let [alive-set (create-alive-set in-str)]
    (loopr [ok 0]
           [cell alive-set]
           (recur (if (> 4 (check-surrounding-count alive-set cell))
                    (inc ok)
                    ok)))))

(comment (solve1 (slurp "./inputs/day4.txt")))

(defn find-removable [alive-set]
  (filter
   #(> 4 (check-surrounding-count alive-set %))
   alive-set))

(defn solve2 [in-str]
  (loop [alive-set (create-alive-set in-str)
         total 0]
    (let [removable (find-removable alive-set)
          n (count removable)]
      (if (pos? n)
        (recur (reduce disj alive-set removable) (+ total n))
        total))))

(comment (solve2 input1))

(comment (let [inp (slurp "./inputs/day4.txt")]
           (c/quick-bench (solve2 inp))))

