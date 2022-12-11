(ns advent.2022.day9
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def test-input "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2")

(defn parse-input [input]
  (->> input
       str/split-lines
       (mapcat #(repeat (Integer/parseInt (last (str/split % #" "))) (first %)))))

(def test-commands (parse-input test-input))

(defn step [[h-pos t-pos dirs visited] dir]
  (let [[hx hy] h-pos
        [tx ty] t-pos
        [hx' hy' :as new-h-pos] (case dir
                                  \U [hx (dec hy)]
                                  \D [hx (inc hy)]
                                  \L [(dec hx) hy]
                                  \R [(inc hx) hy])
        x-delta (Math/abs (- hx' tx))
        y-delta (Math/abs (- hy' ty))
        new-t-pos (if (#{\U \D} dir)
                    (if (< 1 y-delta)
                      [hx (+ ty (/ (- hy' ty) y-delta))]
                      [tx ty])
                    (if (< 1 x-delta)
                      [(+ tx (/ (- hx' tx) x-delta)) hy]
                      [tx ty]))]
        [new-h-pos new-t-pos  (conj dirs dir) (conj visited new-t-pos)]))


(def input (->> "2022/day9-input1.txt"
                io/resource
                slurp))



(defn print-grid [[H T dirs visited]]
  (println "-----------")
  (println (last dirs))
  (let [xs (map first visited)
        ys (map second visited)
        min-x (if (not-empty xs) (apply min xs) 0)
        max-x (if (not-empty xs) (apply max xs) 0)
        min-y (if (not-empty ys) (apply min ys) 0)
        max-y (if (not-empty ys) (apply max ys) 0)
        v-set (set visited)
        ]
    (doseq [y (range -6 6)
            x (range -6 6)]
      (cond (= [x y] H) (print "H")
            (= [x y] T) (print "T")
            (v-set [x y]) (print "#")
            :else (print "."))
      (when  (= x 5) (print "\n")))
    (flush)))

(comment

  (doseq [result (reductions step [[0 0] [0 0] [] []] (parse-input test-input))]
    (print-grid result))
  )

(defn solve [input]
  (let [[H T dirs visited]
        (reduce step [[0 0] [0 0] [] []] (parse-input input))]
    (->> (set visited) count)))
