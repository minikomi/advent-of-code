(ns advent.2017.day19
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def input-raw
  (slurp (io/resource "day19.txt")))

(def test-input
  (slurp (io/resource "day19test.txt")))

(def dirs
  {:up    [0 -1]
   :down  [0 1]
   :right [1 0]
   :left  [-1 0]})

(defn move [[x y] dir]
  (let [[dx dy] (dirs dir)]
    [(+ x dx)
     (+ y dy)]))

(defn test-dir [m pos dir]
  (when (get m (move pos dir)) dir))

(defn get-new-direction [dir m pos]
  (case dir
    (:left :right)
    (some #(test-dir m pos %) [:up :down])
    (:up :down)
    (some #(test-dir m pos %) [:left :right])))

(defn make-pos-map [input-lines]
  (into {}
        (for [y (range (count input-lines))
              x (range (count (first input-lines)))
              :let [c (get-in input-lines [y x])]
              :when (not= \space c)]
          [[x y] c])))

(defn solve [input]
  (let [input-lines (vec (map vec (s/split-lines input)))
        m (make-pos-map input-lines)
        start-pos (.indexOf (first input-lines) \|)]
    (loop [pos [start-pos 0] dir :down seen [] count 0]
      (let [new-pos (move pos dir)
            next-char (get m new-pos)]
        (if next-char
          (recur new-pos
                 (cond-> dir
                   (= \+ next-char) (get-new-direction m new-pos))
                 (cond-> seen
                   (Character/isLetter next-char) (conj next-char))
                 (inc count))
          [(apply str seen) (inc count)])))))
