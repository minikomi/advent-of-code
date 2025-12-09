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

(defn resolve-t-pos [[hx hy] [tx ty]]
  (let [x-delta (- hx tx)
        y-delta (- hy ty)]
    (if (or (#{2 -2} x-delta)
            (#{2 -2} y-delta))
      [(cond (> 0 x-delta) (dec tx)
             (< 0 x-delta) (inc tx)
             :else tx)
       (cond (> 0 y-delta) (dec ty)
             (< 0 y-delta) (inc ty)
             :else ty)]
      [tx ty])))

(comment
  (resolve-t-pos [2 1] [0 0])
  ;;
  )

(defn adjust-h-pos [[hx hy] dir]
  (case dir
    \U [hx (dec hy)]
    \D [hx (inc hy)]
    \L [(dec hx) hy]
    \R [(inc hx) hy]))

(defn step [[h-pos t-pos dirs visited] dir]
  (let [new-h-pos (adjust-h-pos h-pos dir)
        new-t-pos (resolve-t-pos new-h-pos t-pos)]
    [new-h-pos new-t-pos (conj dirs dir) (conj visited new-t-pos)]))

(def input (->> "2022/day9-input1.txt"
                io/resource
                slurp))

(defn print-grid [[H T dirs visited]]
  (println "-----------")
  (println (last dirs))
  (let [v-set (set visited)]
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
  ;;
  )

(defn solve [input]
  (let [[H T dirs visited]
        (reduce step [[0 0] [0 0] [] []] (parse-input input))]
    (->> (set visited) count)))

(comment
  (solve input)
  ;;
  )

(defn step-2 [[snake visited] dir]
  (let [new-head (adjust-h-pos (first snake) dir)
        new-snake (reduce (fn [new-snake next-segment]
                            (conj new-snake
                                  (resolve-t-pos (peek new-snake) next-segment)))
                          [new-head]
                          (rest snake))]
    [new-snake (conj visited (peek new-snake))]))

(def larger-test-input "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20")

(defn print-grid2 [[snake visited]]
  (println "-----------")
  (let [snakes (set snake)]
    (doseq [y (range -20 20)
            x (range -20 20)]
      (cond (snakes [x y]) (print (.indexOf snake [x y]))
            (visited [x y]) (print "#")
            :else (print "."))
      (when  (= x 19) (print " " y) (print "\n")))
    (flush)))

(comment
  (def n (atom 10))
  (def pinput (parse-input input))
  (do
    (print @n)
    (println (nth pinput (dec @n)))
    (print-grid2
     (reduce step-2
             [(vec (repeat 10 [0 0])) #{[0 0]}]
             (take @n (parse-input input))))
    (swap! n inc))
  ;;
  )

(defn solve2
  ([input] (solve2 input false))
  ([input debug?]
   (->> input
        str/split-lines
        (reduce (fn [state line]
                  (when debug? (println line))
                  (let [new-state (reduce step-2 state (parse-input line))]
                    (when debug? (print-grid2 new-state))
                    new-state))
                [(vec (repeat 10 [0 0])) #{}])
        second
        count)))

(comment
  (solve2 larger-test-input true)
  (solve2 input)
  ;;
  )
