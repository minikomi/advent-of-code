(ns day6
  (:require
   [clojure.string :as str]
   [criterium.core :as c]
   [dom-top.core :refer [loopr]]
   [instaparse.core :as insta]))

(def input1 "123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  ")


(defn parse [s]
  (let [grammar "
main = numberlines ops
numberlines = numberline+ 
ops = <ws> (op <ws>)+ <'\n'?>
numberline = <ws> number (<ws> number)* <ws> <'\\n'>
op = #'[\\*\\+]'
ws = #'[ \\t]*'
number = #'[0-9]+'"
        parser (insta/parser grammar)
        parse-tree (parser s)]
    (insta/transform
     {:main  (fn [numberlines ops] {:numberlines numberlines :ops ops})
      :numberlines vector
      :ops vector
      :op {"*" *
           "+" +}
      :numberline vector
      :number clojure.edn/read-string}
     parse-tree)))

(comment
  (parse input1)
  ;;
  )

(defn solve1 [{:keys [numberlines ops]}]
  (->> numberlines
       (apply mapv vector)
       (map apply ops)
       (reduce +)))

(comment (solve1 (parse input1))
         (solve1 (parse (str/trim (slurp "./inputs/day6.txt")))))

(comment
  (solve1 (slurp "./inputs/day1.txt")))

(defn parse-col-set [cols]
  (let [op (if (str/includes? (first cols) "*") * +)
        numbers (map #(clojure.edn/read-string (str/replace % #"[^\d\s]" "")) cols)]
    (apply op numbers)))

(defn solve2 [inp-str]
  (let [txt-columns (apply map str (str/split-lines inp-str))]
    (loopr
        [current [] total 0]
        [col txt-columns]
        (if (str/blank? col)
          (recur [] (+ total (parse-col-set current)))
          (recur (conj current col) total))
      (+ total (parse-col-set current)))))

(solve2 (slurp "./inputs/day6.txt"))
