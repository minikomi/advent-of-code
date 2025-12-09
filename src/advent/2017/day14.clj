(ns advent.2017.day14
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [advent.2017.day10 :as day10]
            [advent.util :as util]))

(def input "nbysizxe")

(defn make-hashes [input-str]
  (map #(day10/knot (str input-str "-" %))
       (range 128)))

(defn gen-square [input-str]
  (->> input-str
       make-hashes
       (mapv
        #(vec (mapcat
               (fn [v] (format "%8s" (Integer/toBinaryString v))) %)))))

(defn solve1 [input-str]
  (-> (mapcat
       (partial apply str)
       (gen-square input-str))
      frequencies
      (get \1)))

(defn sq->pos-sq [sq]
  (into {}
        (for [x (range 128) y (range 128)]
          [[x y] (get-in sq [x y])])))

(defn check-neighbours [sq [x y]]
  (vec
   (keep
    (fn [[x-mod y-mod]]
      (let [new-pos [(+ x x-mod) (+ y y-mod)]]
        (when (= \1 (get sq new-pos)) new-pos)))
    [[-1 0] [1 0] [0 -1] [0 1]])))

(defn get-next-seed [sq]
  (ffirst
   (filter
    (fn [[pos v]]
      (= \1 v))
    sq)))

(defn flood-count [square]
  (loop [gr 1
         sq (assoc (sq->pos-sq square) [0 0] 1)
         stack (check-neighbours sq [0 0])]
    (if (empty? stack)
      (if-let [s (get-next-seed sq)]
        (do
          (recur (inc gr)
                 (assoc sq s (inc gr))
                 (check-neighbours sq s)))
        gr)
      (let [new-sq (assoc sq (peek stack) gr)
            new-stack (into (pop stack)
                            (check-neighbours new-sq (peek stack)))]
        (recur gr
               new-sq
               new-stack)))))

(def test-str "flqrgnkx")

(comment
  (count (first (gen-square "flqrgnkx")))
  (solve1 test-str)
  (solve1 input)
  (flood-count (gen-square test-str))
  (flood-count (gen-square input))i)
