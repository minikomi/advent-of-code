(ns advent.2017.day22
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def dirs
  [[0 -1]  ;; 0 up
   [1 0]   ;; 1 right
   [0 1]   ;; 2 down
   [-1 0]  ;; 3 left
   ])

(defn move [{[^Integer x ^Integer y] :pos dir :dir}]
  (let [[dx dy] (dirs dir)]
    [(+ x dx) (+ y dy)]))

(defn turn [{:keys [mtx pos dir]}]
  (mod
   (case (mtx pos)
     \# (inc dir)
     (dec dir))
   4))

(defn infect [{:keys [pos mtx infected] :as state}]
  (if (= \# (mtx pos))
    (assoc state
           :mtx (assoc mtx pos \.))
    (assoc state
           :mtx (assoc mtx pos \#)
           :infected (inc infected))))

(defn step [state]
  (as-> state $
    (assoc $ :dir (turn $))
    (infect $)
    (assoc $ :pos (move $))))

(def test-mtx "..#
#..
...")

(defn parse-mtx [m]
  (let [lines (mapv vec (s/split-lines m))
        y-count (count lines)
        x-count (count (first lines))]
    {:mtx (into {}
                 (for [y (range y-count)
                       x (range x-count)]
                   [[x y] (get-in lines [y x])]))
     :dir 0
     :infected 0
     :pos [(int (/ x-count 2))
           (int (/ y-count 2))]}))

(def input-raw (slurp (io/resource "day22.txt")))

(comment
  (:infected (nth (iterate step (parse-mtx test-mtx)) 10000)) ;; 5587
  (:infected (nth (iterate step (parse-mtx input-raw)) 10000))
  )
