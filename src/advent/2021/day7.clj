(ns advent.2021.day7
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def test-input "16,1,2,0,4,2,7,1,2,14")

(defn abs [n] (max n (- n)))

(defn parse-input [s]
  (mapv #(Integer/parseInt (str/trim %)) (str/split s #",")))

(def parsed-input (-> "2021/input7.txt" io/resource slurp parse-input))

(def parsed-test-input (parse-input test-input))

(defn distance [n v]
  (abs (- n v)))

(defn find-stop [places]
  (->> (map (fn [n] [n (apply + (map #(distance % n) places))])
            (range (apply max places)))
       (sort-by second)
       first
       first))

(comment
  (find-stop parsed-test-input))

(defn solve [crabs]
  (let [s (find-stop crabs)]
    (->> (map #(distance s %) crabs)
         (reduce +))))

(comment (solve parsed-input))

(defn crab-distance [n v]
  (let [d (abs (- n v))]
    (/ (* d (inc d))
       2)))

(defn find-crab-stop [places]
  (->> (map (fn [n]
              [n (apply + (map #(crab-distance % n) places))])
            (range (apply max places)))
       (sort-by second)
       first
       first))

(comment (find-crab-stop parsed-test-input))

(defn solve-2 [crabs]
  (let [s (find-crab-stop crabs)]
    (->> (map #(crab-distance s %) crabs)
         (reduce +))))

(comment (solve-2 parsed-input))
