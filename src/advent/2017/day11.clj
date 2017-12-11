(ns advent.2017.day11
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [advent.util :as util]))

(def input-raw
  (s/trim (slurp (io/resource "day11.txt"))))

(def get-dir
  {"n" [0 1 -1]
   "s" [0 -1 1]
   "ne" [1 0 -1]
   "nw" [-1 1 0]
   "se" [1 -1 0]
   "sw" [-1 0 1]
   })

(defn step [state action]
  (mapv + state (get-dir action)))

(defn distance [[x y z]]
  (/ (+ (Math/abs x)
        (Math/abs y)
        (Math/abs z))
     2))

(comment (distance (reduce step [0 0 0] ["ne" "ne" "ne"]))

         (distance (reduce step [0 0 0] ["ne","ne","sw","sw"]))

         (distance (reduce step [0 0 0] ["ne","ne","s","s"]))

         (distance (reduce step [0 0 0] ["se","sw","se","sw","sw"]))

         (distance (reduce step [0 0 0] (s/split input-raw #",")))

         (apply max (map distance (reductions step [0 0 0] (s/split input-raw #",")))))
