(ns advent.2021.day4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def input (->> "2021/input4.txt"
                io/resource
                slurp
                ;;
                ))

(def test-input
  "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7")

(defn parse-int [s] (Integer/parseInt s))

(defn parse-input [input]
  (let [[call-s & boards-s] (str/split input #"\n\n")
        call-order (mapv parse-int (str/split call-s #","))
        boards (mapv (fn [b]
                       (mapv #(mapv parse-int (str/split (str/trim %) #" +"))
                             (str/split-lines b)))
                     boards-s)]
    {:call-order call-order
     :boards boards}))

(def parsed-test (parse-input test-input))
(def parsed-input (parse-input input))

(defn transpose [rows]
  (apply mapv vector rows))

(defn all-coords [size]
  (mapv
   (fn [y] (mapv #(vector y %) (range size)))
   (range size)))

(defn gen-win-states [size]
  (let [horiz (all-coords size)]
    (mapv set (into horiz (transpose horiz)))))

(defn gen-board-state [board]
  (let [board-size (count board)
        co-ords (reduce into [] (all-coords board-size))]
    {:indexes (->> co-ords
                   (map #(vector (get-in board %) %))
                   (into {}))
     :unmarked (gen-win-states board-size)}))

(defn mark-board [{:keys [indexes unmarked] :as board-state} n]
  (if-let [idx (indexes n)]
    (assoc board-state
           :unmarked
           (mapv #(disj % idx) unmarked)
           :indexes
           (dissoc indexes n))
    board-state))

(defn bingo? [{:keys [unmarked]}] (some empty? unmarked))

(defn find-first [f coll]
  (first (drop-while (complement f) coll)))

(defn find-bingo [{:keys [call-order boards]}]
  (loop [board-states (mapv gen-board-state boards)
         seen []
         remain call-order]
    (if (empty? remain) nil
        (let [n (first remain)
              new-board-states (mapv #(mark-board % n) board-states)
              new-seen (conj seen n)]
          (if-let [bingo (find-first bingo? new-board-states)]
            [new-seen bingo]
            (recur new-board-states new-seen (rest remain)))))))

(defn advent-1 [bingo-game]
  (if-let [[seen board] (find-bingo bingo-game)]
    (let [unseen (keys (:indexes board))
          unseen-sum (reduce + unseen)]
      (* (last seen) unseen-sum))))

(defn find-last-bingo [{:keys [call-order boards]}]
  (loop [board-states (mapv gen-board-state boards)
         seen []
         bingo-boards []
         remain call-order]
    (cond (or (empty? remain)
              (empty? board-states)) [seen (last bingo-boards)]
          :else (let [n (first remain)
                      new-board-states (mapv #(mark-board % n) board-states)
                      new-seen (conj seen n)
                      bingo-d (group-by bingo? new-board-states)]
                  (recur (get bingo-d nil)
                         new-seen
                         (into bingo-boards (get bingo-d true))
                         (rest remain))))))

(defn advent-2 [bingo-game]
  (if-let [[seen board] (find-last-bingo bingo-game)]
    (let [unseen (keys (:indexes board))
          unseen-sum (reduce + unseen)]
      (* (last seen) unseen-sum))))

(comment (advent-2 parsed-input))
