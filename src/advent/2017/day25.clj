(ns advent.2017.day25
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.pprint :refer [pprint]]
            [clojure.set :as set]))

(defn test1 []
  (loop [n 0
         tape {}
         pos 0
         state :a]
    (if (>= n 6)
      (apply + (vals tape))
      (let [v (get tape pos 0)]
        (case [state v]
          [:a 0]
          (recur (inc n)
                 (assoc tape pos 1)
                 (inc pos)
                 :b)
          [:a 1]
          (recur (inc n)
                 (assoc tape pos 0)
                 (dec pos)
                 :b)
          [:b 0]
          (recur (inc n)
                 (assoc tape pos 1)
                 (dec pos)
                 :a)
          [:b 1]
          (recur (inc n)
                 (assoc tape pos 1)
                 (inc pos)
                 :a))))))

(defn solve1 []
  (loop [n 0
         tape {}
         pos 0
         state :a]
    (if (>= n 12134527)
      (apply + (vals tape))
      (let [v (get tape pos 0)]
        (case [state v]
          ;; A
          ;; If the current value is 0:
          ;; - Write the value 1.
          ;; - Move one slot to the right.
          ;; - Continue with state B.
          [:a 0]
          (recur (inc n)
                 (assoc tape pos 1)
                 (inc pos)
                 :b)
          ;; If the current value is 1:
          ;; - Write the value 0.
          ;; - Move one slot to the left.
          ;; - Continue with state C.
          [:a 1]
          (recur (inc n)
                 (assoc tape pos 0)
                 (dec pos)
                 :c)
          ;; B
          ;; If the current value is 0:
          ;; - Write the value 1.
          ;; - Move one slot to the left.
          ;; - Continue with state A.
          [:b 0]
          (recur (inc n)
                 (assoc tape pos 1)
                 (dec pos)
                 :a)
          ;; If the current value is 1:
          ;; - Write the value 1.
          ;; - Move one slot to the right.
          ;; - Continue with state C.
          [:b 1]
          (recur (inc n)
                 (assoc tape pos 1)
                 (inc pos)
                 :c)
          ;; C
          ;; If the current value is 0:
          ;; - Write the value 1.
          ;; - Move one slot to the right.
          ;; - Continue with state A.
          [:c 0]
          (recur (inc n)
                 (assoc tape pos 1)
                 (inc pos)
                 :a)
          ;; If the current value is 1:
          ;; - Write the value 0.
          ;; - Move one slot to the left.
          ;; - Continue with state D.
          [:c 1]
          (recur (inc n)
                 (assoc tape pos 0)
                 (dec pos)
                 :d)
          ;; D
          ;; If the current value is 0:
          ;; - Write the value 1.
          ;; - Move one slot to the left.
          ;; - Continue with state E.
          [:d 0]
          (recur (inc n)
                 (assoc tape pos 1)
                 (dec pos)
                 :e)
          ;; If the current value is 1:
          ;; - Write the value 1.
          ;; - Move one slot to the left.
          ;; - Continue with state C.
          [:d 1]
          (recur (inc n)
                 (assoc tape pos 1)
                 (dec pos)
                 :c)
          ;; E
          ;; If the current value is 0:
          ;; - Write the value 1.
          ;; - Move one slot to the right.
          ;; - Continue with state F.
          [:e 0]
          (recur (inc n)
                 (assoc tape pos 1)
                 (inc pos)
                 :f)
          ;; If the current value is 1:
          ;; - Write the value 1.
          ;; - Move one slot to the right.
          ;; - Continue with state A.
          [:e 1]
          (recur (inc n)
                 (assoc tape pos 1)
                 (inc pos)
                 :a)
          ;; F
          ;; If the current value is 0:
          ;; - Write the value 1.
          ;; - Move one slot to the right.
          ;; - Continue with state A.
          [:f 0]
          (recur (inc n)
                 (assoc tape pos 1)
                 (inc pos)
                 :a)
          ;; If the current value is 1:
          ;; - Write the value 1.
          ;; - Move one slot to the right.
          ;; - Continue with state E.
          [:f 1]
          (recur (inc n)
                 (assoc tape pos 1)
                 (inc pos)
                 :e)
          )))))

(comment
  (solve1)
  )
