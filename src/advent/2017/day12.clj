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

(defn input->pipe-map [input]
  (->> input s/split-lines (map parse-row) (into {})))

(defn solve1 [pipe-map start]
  (loop [acc (transient #{})
         stack (get pipe-map start)]
    (if-let [p (peek stack)]
      (let [joined (remove acc (pipe-map p))]
        (recur (reduce conj! acc joined)
               (into (pop stack) joined)))
      (persistent! acc))))

(comment
  (count (solve1 (input->pipe-map test-input) 0))
  (count (solve1 (input->pipe-map input-raw) 0))
  )

(defn solve2 [pipe-map]
  (loop [remaining-programs (set (keys pipe-map))
         groups {}]
    (if (empty? remaining-programs) groups
        (let [start (first remaining-programs)
              visited (solve1 pipe-map start)]
          (recur (set/difference remaining-programs visited)
                 (assoc groups start visited))))))

(comment
  (time (count (solve2 (input->pipe-map test-input))))
  (time (count (solve2 (input->pipe-map input-raw)))))
  )
