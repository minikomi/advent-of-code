(import ./util :reload-all)
(use judge)

(def input1 `0123
1234
8765
9876`)

(def trail-grammar
  (peg/compile ~{:pos (/ (* (line) (column)) ,tuple)
                 :char (* :pos (number :d))
                 :main (/ (some (+ :s "." :char)) ,table)}))

(defn parse [input]
  (first (peg/match trail-grammar input))
  )

(defn find-trailheads [mtx]
  (var col @[])
  (eachp [k v] mtx
    (when (= 0 v) (array/push col k)))
  col)

(test (find-trailheads (parse input1)) @[[1 1]])

(defn tree-seq [branch? children root]
  (fiber/new 
    (fn []
      (defn walk [node]
        (yield node)
        (when (branch? node)
          (each child (children node)
            (walk child))))
      (walk root))))

(defn next-steps [mtx pos]
  (seq [:let [current (mtx pos)]
        :when (not (nil? current))
         n :in (map |(util/add-v pos $) [[-1 0] [1 0] [0 -1] [0 1]])
        :when (= (inc current) (mtx n))]
    n))


(test (next-steps (parse input1) [1 1]) @[[2 1] [1 2]])

(def input2 `..90..9
...1.98
...2..7
6543456
765.987
876....
987....`)

(def input3 `89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732`)

(defn solve1 [input-str]
  (let [counts @{}
        mtx (parse input-str)
        branch? (fn [v]
                  (cond (= v :start) true
                        (= 9 (mtx (v :p)))
                        (do
                          (put counts
                               [(v :p) (v :start)]
                               true)
                          false)
                        :else true))
        children (fn [pos]
                   (if (= :start pos)
                     (map (fn [p] {:start p :p p}) (find-trailheads mtx))
                     (map (fn [p] {:start (pos :start) :p p})
                          (next-steps mtx (pos :p)))))]
    (seq [i :in (tree-seq branch? children :start)] i)
    counts))

(test (length (solve1 input2)) 4)
(test (length (solve1 input3)) 7)

(def day10-input (util/read-file "./resources/day10.txt"))

(test (length (solve1 day10-input)) 652)

(defn solve2 [input-str]
  (let [counts @{}
        mtx (parse input-str)
        branch? (fn [v]
                  (cond (= v :start) true
                        (= 9 (mtx (v :p)))
                        (do
                          (put counts
                               (v :start)
                               (inc (or (counts (v :start)) 0)))
                          false)
                        :else true))
        children (fn [pos]
                   (if (= :start pos)
                     (map (fn [p] {:start p :p p}) (find-trailheads mtx))
                     (map (fn [p] {:start (pos :start) :p p})
                          (next-steps mtx (pos :p)))))]
    (seq [i :in (tree-seq branch? children :start)] i)
    counts))

(def input4 `012345
123456
234567
345678
4.6789
56789.`)

(test (+ ;(values (solve2 input3))) 81)
(test (+ ;(values (solve2 input4))) 227)
(test (+ ;(values (solve2 day10-input))) 1432)
