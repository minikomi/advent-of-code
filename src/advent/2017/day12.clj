(ns advent.2017.day12
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [advent.util :as util]))

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

(defn solve1 [input]
  (let [pipe-map (->> input s/split-lines (map parse-row) (into {}))]
    (loop [acc #{} stack (get pipe-map 0)]
      (if (empty? stack) acc
          (let [joined (filter #(not (acc %))
                               (get pipe-map (peek stack)))]
            (recur (into acc joined)
                   (into (pop stack) joined)))))))
