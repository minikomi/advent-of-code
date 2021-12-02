(ns advent.2021.day2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (->> (io/resource "2021/input2.txt")
                slurp
                str/split-lines
                ;;
                ))

(def test-input-1
  (str/split-lines "forward 5
down 5
forward 8
up 3
down 8
forward 2"))

(defn step [state row]
  (let [[command n-str] (str/split row #" ")
        n (Integer/parseInt n-str)]
    (case command
      "forward" (update state :h + n)
      "up" (update state :v - n)
      "down" (update state :v + n))))

(comment
  (step {:h 0 :v 0} "down 1"))

(defn advent-1 [vs]
  (let [start {:h 0 :v 0}
        moved (reduce step start vs)]
    (* (:h moved) (:v moved))))

(comment
  (advent-1 test-input-1)
  (advent-1 input))

(defn step2 [state row]
  (let [[command n-str] (str/split row #" ")
        n (Integer/parseInt n-str)]
    (case command
      "forward"
      (-> state
          (update :h + n)
          (update :v + (* n (:aim state))))
      "up"
      (update state :aim - n)
      "down"
      (update state :aim + n))))

(defn advent-2 [vs]
  (let [start {:h 0 :v 0 :aim 0}
        moved (reduce step2 start vs)]
    (* (:h moved) (:v moved))))

(comment
  (advent-2 test-input-1)
  (advent-2 input)
  ;;
  )
