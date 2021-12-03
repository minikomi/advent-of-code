(ns advent.2021.day3
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (->> "2021/input3.txt"
                io/resource
                slurp
                str/split-lines
                ;;
                ))

(def test-input-1
  (str/split-lines "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010"))

(defn transpose [rows]
  (apply mapv vector rows))

(defn parse-binary [s]
  (Integer/parseInt s 2))

(defn advent-1 [rows]
  (->> (for [t (transpose rows)
             :let [f (frequencies t)]]
         (map first (sort-by second f)))
       (transpose)
       (map #(apply str %))
       (map parse-binary)
       (apply *)))

(comment
  (advent-1 test-input-1)
  (advent-1 input))

(defn advent-2 [input rating-type]
  (let [[main-bit sub-bit] (if (= rating-type :oxygen) [\1 \0] [\0 \1])]
    (loop [remain input n 0]
      (if (<= (count remain) 1) (first remain)
          (let [f (frequencies (-> remain transpose (get n)))
                f0 (get f \0)
                f1 (get f \1)]
            (recur
             (if (<= f0 f1)
               (filter #(= main-bit (get % n)) remain)
               (filter #(= sub-bit (get % n)) remain))
             (inc n)))))))

(comment
  (* (parse-binary (advent-2 test-input-1 :oxygen))
     (parse-binary (advent-2 test-input-1 :co2))))
