(ns day2
  (:require
   [clojure.string :as str]
   [instaparse.core :as insta]))

(def input1 "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,
1698522-1698528,446443-446449,38593856-38593862,565653-565659,
824824821-824824827,2121212118-2121212124")

(defn parse [s]
  (let [grammar "<ranges> = range+
                   range = number <'-'> number <','?> <'\n'?>
                   number = #'[0-9]+'"
        parser (insta/parser grammar)
        parse-tree (parser s)]
    (insta/transform
     {:range (fn [a b] (range a (inc b)))
      :number clojure.edn/read-string}
     parse-tree)))

(defn valid-id? [id]
  (let [id (str id)]
    (cond (odd? (count id)) true
          :else (let [mid-point (/ (count id) 2)
                      partitioned (partition mid-point id)]
                  (apply distinct? partitioned)))))

(comment
  (parse input1)

  (for [d ["11" "010" "123" "1010" "446446" "1011"]]
    [d (valid-id? d)]))

(defn solve1 [input-str]
  (->> (parse input-str)
       (map #(remove valid-id? %))
       flatten
       (reduce +)))

(comment
  (solve1 (slurp "./inputs/day2.txt")))

(defn repeated-block? [s]
  (let [n (count s)]
    (and (pos? n)
         (let [pos (.indexOf (str s s) s 1)]
           (and (>= pos 1) (< pos n))))))

(defn valid-id-2? [id]
  (let [s (str id)
        n (count s)]
    (cond
      (< n 2) true
      (= \0 (first s)) false
      :else (not (repeated-block? s)))))

(valid-id-2? "123123123123")

(["12" "12" "12"])

(comment
  (map (juxt identity valid-id-2?)
       ["11"
        "123123123" "12121212" "1112111211131112"
        "123124123"]))

(defn solve2 [input-str]
  (->> (parse input-str)
       (map #(remove valid-id-2? %))
       (flatten)
       (apply +)))

(comment (solve2 input1))

(comment
  (solve2 (slurp "./inputs/day2.txt")))

