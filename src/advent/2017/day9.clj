(ns advent.2017.day9
  (:require [clojure.java.io :as io]))

(def input (slurp (io/resource "day9.txt")))

(defn solve1 [input]
  (loop [s     (vec (reverse input))
         state {:lvl     0
                :garbage false
                :gc      0
                :total   0}]
    (if-let [c (peek s)]
      (if (:garbage state)
        (case c
          \> (recur (pop s) (assoc state :garbage false))
          \! (recur (pop (pop s)) state)
          (recur (pop s) (update state :gc inc)))
        (case c
          \{ (recur (pop s) (update state :lvl inc))
          \} (recur (pop s) (-> state
                                (update :lvl dec)
                                (update :total + (:lvl state))))
          \< (recur (pop s) (assoc state :garbage true))
          (recur (pop s) state)))
      state)))

(comment
  (solve1 "{}")
  (solve1 "{{{}}}")
  (solve1 "{{<!!>},{<!!>},{<!!>},{<!!>}}")
  (solve1 "{{<a!>},{<a!>},{<a!>},{<ab>}}")

  (solve1 "<{o\"i!a,<{i<a>") ;; gc 10

  (solve1 input))
