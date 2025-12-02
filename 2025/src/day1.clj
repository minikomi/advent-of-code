(ns day1
  (:require
   [clojure.string :as str]
   [instaparse.core :as insta]))

(def input1 "L68
L30
R48
L5
R60
L55
L1
L99
R14
L82")

(def dial-start 50)

(defn parse [s]
  (let [grammar "<instructions> = instruction+
                   instruction = turn number <'\n'?>
                   turn = 'L' | 'R'
                   number = #'[0-9]+'"
        parser (insta/parser grammar)
        parse-tree (parser s)]
    (insta/transform
     {:instruction vector
      :number clojure.edn/read-string
      :turn {"L" :- "R" :+}}
     parse-tree)))

(defn run [instructions]
  (reductions (fn [total [op-key n]]
                (let [op ({:+ + :- -} op-key)]
                  (mod (op total n) 100)))
              dial-start
              instructions))

(comment
  (parse input1))

(defn solve1 [input-str]
  (count (filter zero? (run (parse input-str)))))

(assert (= 3 (solve1 input1)))

(comment
  (solve1 (slurp "./inputs/day1.txt")))

(defn run2 [instructions]
  (reduce
   (fn [[total zero-pass-count] [op-key n]]
     (let [op ({:+ + :- -} op-key)
           full-spins (quot n 100)          ;; full spins in n
           moved     (op total (mod n 100)) ;; raw
           new-total (mod moved 100)      ;; correct dial position

           zero-passes (case op-key
                         ;; Addition
                         ;; If we crossed zero during addition, count how many times
                         :+ (cond
                              (zero? total) full-spins
                              (= new-total 0) (inc full-spins)
                              (< new-total moved) (inc full-spins)
                              :else full-spins)

                         ;; Subtraction
                         :- (cond
                              (zero? total) full-spins
                              (= new-total 0) (inc full-spins)
                              (> new-total moved) (inc full-spins)
                              :else full-spins)

                         0)
           new-zero-pass-count (+ zero-pass-count zero-passes)]

       [new-total new-zero-pass-count]))
   [dial-start 0]
   (filter #(not= 0 (second %)) instructions)))

(assert (= [32 6] (run2 (parse input1))))

(defn solve2 [input-str]
  (second (run2 (parse input-str))))

(comment
  (solve2 (slurp "./inputs/day1.txt")))
