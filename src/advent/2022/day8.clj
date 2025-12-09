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
  (solve input)
  ;;
  )

(defn create-map [input]
  (let [grid (parse-input input)
        all-nodes (reduce into [] grid)]
    {:trees (into {} all-nodes)
     :w (count (first grid))
     :h (count grid)}))

(def test-map (create-map test-input))

(defn dir-score [{:keys [trees w h]} start dir]
  (let [v (get trees start)]
    (loop [[y x] start n 0]
     (let [[new-y new-x :as new-pos] (case dir
                     :up [(dec y) x]
                     :down [(inc y) x]
                     :left [y (dec x)]
                     :right [y (inc x)])]
         (cond (or (> 0 new-x) (= w new-x)
                   (> 0 new-y) (= h new-y)) n
               (<= v (get trees new-pos)) (inc n)
               :else (recur new-pos (inc n)))))))

(defn tree-score [tree-map start]
  (apply * (map #(dir-score tree-map start %) [:up :left :down :right])))

(defn solve2 [input]
  (apply max (let [tree-map (create-map input)]
     (map #(tree-score tree-map %)  (keys (:trees tree-map))))))
