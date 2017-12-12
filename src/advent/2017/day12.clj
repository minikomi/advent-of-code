(ns advent.2017.day12
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [advent.util :as util]
            [clojure.set :as set]))

(def input-raw
  (s/trim (slurp (io/resource "day12.txt"))))

(def test-input
  "0 <-> 2
  1 <-> 1
  2 <-> 0, 3, 4
  3 <-> 2, 4
  4 <-> 2, 3, 6
  5 <-> 6
  6 <-> 4, 5")

(defn parse-row [row]
  (let [[in-raw out-raw] (s/split (s/trim row) #" <-> ")]
    [(read-string in-raw)
     (read-string (str "[" out-raw "]"))]))

(defn solve1 [pipe-map start]
  (loop [acc #{} stack (get pipe-map start)]
    (if (empty? stack) acc
        (let [joined (->> (peek stack)
                          (pipe-map)
                          (filter #(not (acc %))))]
          (recur (into acc joined)
                 (into (pop stack) joined))))))

(defn solve2 [input]
  (let [pipe-map (->> input s/split-lines (map parse-row) (into {}))
        all-programs (reduce into (set (keys pipe-map)) (vals pipe-map))]
    (loop [remaining-programs all-programs groups {}]
      (if (empty? remaining-programs) groups
          (let [start (first remaining-programs)
                visited (solve1 pipe-map (first remaining-programs))]
            (recur (set/difference remaining-programs visited)
                   (assoc groups start visited)))))))
