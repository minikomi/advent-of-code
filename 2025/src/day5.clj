(ns day5
  (:require
   [clojure.string :as str]
   [dom-top.core :refer [loopr]]
   [instaparse.core :as insta]))

(def input1 "3-5
10-14
16-20
12-18

1
5
8
11
17
32")

(defn parse [s]
  (let [grammar "
main = idranges <newline+> numbers
idranges = idrange (<newline> idrange)*
idrange = number <'-'> number
numbers = number (<newline> number)*
newline = #'\n'
number = #'[0-9]+'"
        parser (insta/parser grammar)
        parse-tree (parser s)]
    (insta/transform
     {:main (fn [id ns] {:idranges id :numbers ns})
      :idrange vector
      :idranges vector
      :numbers vector
      :number clojure.edn/read-string}
     parse-tree)))

(parse input1)

(defn fresh-check [{:keys [idranges numbers]}]
  (filter #(some (fn [[a b]] (and (>= % a) (<= % b))) idranges) numbers))

(comment
  (fresh-check (parse input1)))

(comment
  (parse (str/trim (slurp "./inputs/day5.txt"))))

(comment
  (count (fresh-check (parse (str/trim (slurp "./inputs/day5.txt"))))))

(defn solve2 [{:keys [idranges]}]
  (let [[[a b] & more] (sort-by first idranges)]
    (loopr [[cur-start cur-end] [a b]
            total 0]
           [[na nb] more]
           (if (<= na (inc cur-end))
             (recur [cur-start (max cur-end nb)] total)
             (recur [na nb] (+ total (- (inc cur-end) cur-start))))
           (+ total (- (inc cur-end) cur-start)))))

(comment
  (solve2 (parse input1)))
(comment
  (solve2 (parse (str/trim (slurp "./inputs/day5.txt")))))

