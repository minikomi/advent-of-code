(ns advent.2017.day19
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [advent.util :as util]))

(def input-raw
  (slurp (io/resource "day19.txt")))

(def dirs
  {:up [0 -1]
   :down [0 1]
   :right [1 0]
   :left [-1 0]})

(defn not-opposite [d1 d2]
  (case [d1 d2]
    [:down :up] false
    [:up :down] false
    [:left :right] false
    [:right :left] false
    true
    ))


(defn move [[x y] dir]
  (let [[dx dy] (dirs dir)]
    [(+ x dx)
     (+ y dy)]))

(def test-input
  (slurp (io/resource "day19test.txt")))

(defn get-char-neighbour [m pos dir]
  (first
   (for [[d _] dirs
         :let [new-pos (move pos d)]
         :when (and
                (not-opposite d dir)
                (get m new-pos))]
     d)))

(defn make-pos-map [input-lines]
  (into {}
        (for [y (range (count input-lines))
              x (range (count (first input-lines)))]
          [[x y] (let [c (get-in input-lines [y x])]
                   (when (not= \space c) c))])))

(defn solve [input]
  (let [input-lines (vec (map vec (s/split-lines input)))
        m (make-pos-map input-lines)
        start-pos (.indexOf (first input-lines) \|)]
    (loop [pos [start-pos 0]
           dir :down
           seen []
           count 0]
      (let [new-pos (move pos dir)
            next-char (get m new-pos)]
        (if next-char
          (recur new-pos dir (if (Character/isLetter next-char)
                               (conj seen next-char)
                               seen)
                 (inc count))
          (if-let [char-neighbour-dir (get-char-neighbour m pos dir)]
            (recur pos char-neighbour-dir seen count)
            [(apply str seen) (inc count)]))))))
