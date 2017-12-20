(ns advent.2017.day20
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def input-raw
  (slurp (io/resource "day20.txt")))

(def test-input
  (slurp (io/resource "day20test.txt")))

(defn manhattan [v]
  (reduce #(+ % (Math/abs %2)) 0 v))

(defn magnitude [v]
  (Math/pow (apply + (map #(Math/pow % 2) v))
            0.5))

(def test-line "p=< 4,0,0>, v=< 0,0,0>, a=<-2,0,0>")

(def line-regex #"p=<(.*)>, v=<(.*)>, a=<(.*)>")

(defn parse-line [n line]
  (let [[_ p v a] (re-matches line-regex line)]
    {:n n
     :pos (read-string (str "[" p "]"))
     :vel (read-string (str "[" v "]"))
     :acc (read-string (str "[" a "]"))
     }))

(defn solve1 [input]
  (let [ps (vec (map-indexed parse-line (s/split-lines input)))
        stepped (iterate #(mapv step %) ps)]
    (doseq [n (range 500)]
      (doseq [p (take 10 (sort-by #(magnitude (:acc %)) (nth stepped n)))]
        (println n p (manhattan p))))))

(def test-input-2 "p=<-6,0,0>, v=< 3,0,0>, a=< 0,0,0>
p=<-4,0,0>, v=< 2,0,0>, a=< 0,0,0>
p=<-2,0,0>, v=< 1,0,0>, a=< 0,0,0>
p=< 3,0,0>, v=<-1,0,0>, a=< 0,0,0>")

(defn step-remove-collided [ps]
  (->> ps
       (mapv step)
       (group-by :pos)
       (filter #(> 2 (count (second %))))
       (mapv (comp first second))))

(defn solve2 [input c]
  (let [ps (vec (map-indexed parse-line (s/split-lines input)))
        stepped (iterate step-remove-collided ps)]
    (doseq [n (range c)]
      (println (count (nth stepped n))))))
