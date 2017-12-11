(ns advent.2017.day9
  (:require [clojure.java.io :as io]))

(def input (slurp (io/resource "day9.txt")))

(defn solve1 [input]
  (loop [s       (vec (reverse input))
         lvl     0
         garbage false
         gc      0
         total   0]
    (if-let [c (peek s)]
      (if garbage
        (case c
          \> (recur (pop s) lvl false gc total)
          \! (recur (pop (pop s)) lvl garbage gc total)
          (recur (pop s) lvl true (inc gc) total))
        (case c
          \{       (recur (pop s) (inc lvl) garbage gc total)
          \}       (recur (pop s) (dec lvl) garbage gc (+ total lvl))
          \<       (recur (pop s) lvl true gc total)
          \,       (recur (pop s) lvl garbage gc total)
          \newline (recur (pop s) lvl garbage gc total)
          \!       (recur (pop (pop s)) lvl garbage gc total)
          (throw (ex-info "weird state" {:s s :lvl lvl :total total}))))
      [garbage gc lvl total])))

(comment
  (solve1 "{}")
  (solve1 "{{{}}}")
  (solve1 "{{<!!>},{<!!>},{<!!>},{<!!>}}")
  (solve1 "{{<a!>},{<a!>},{<a!>},{<ab>}}")

  (solve1 "<{o\"i!a,<{i<a>") ;; gc 10

  (solve1 input))
