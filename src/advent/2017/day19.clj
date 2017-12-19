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

(def all-dirs (set (map first dirs)))

(defn move [[x y] dir]
  (let [[dx dy] (dirs dir)]
    [(+ x dx)
     (+ y dy)]))

(defn get-new-direction [m pos dir]
  (first
   (for [[d _] (disj all-dirs dir)
         :let [new-pos (move pos d)]
         :when (get m new-pos)]
     d)))

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
                 (if (= \+ next-char) (get-new-direction m pos dir) dir)
                 (if (Character/isLetter next-char) (conj seen next-char) seen)
                 (inc count))
          [(apply str seen) (inc count)])))))
