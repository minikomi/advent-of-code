(import ./util :reload-all)
(use judge)

(def input1 `190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20`)

(def day7-input (util/read-file "resources/day7.txt"))

(def grammar
  ~{
    :line (group
            (* (number :d+)
               ":"
               (/ (some (* " " (number :d+))) ,tuple)
               (? "\n")))
    :main (some :line)
    })

(test (peg/match grammar input1)
  @[@[190 [10 19]]
    @[3267 [81 40 27]]
    @[83 [17 5]]
    @[156 [15 6]]
    @[7290 [6 8 6 15]]
    @[161011 [16 10 13]]
    @[192 [17 8 14]]
    @[21037 [9 7 18 13]]
    @[292 [11 6 16 20]]])


(defn combinations-gen [xs n]
  (if (zero? n)
    [[]]
    (generate [x :in xs
               ys :in (combinations-gen xs (dec n))]
      (tuple/join [x] ys))))

(test (seq [c :in (combinations-gen [1 2] 3)] c)
  @[[1 1 1]
    [1 1 2]
    [1 2 1]
    [1 2 2]
    [2 1 1]
    [2 1 2]
    [2 2 1]
    [2 2 2]])

(defn find-operators [operators sum values]
  (def gen (combinations-gen operators (dec (length values))))

  (var found nil)
  (each ops gen
    (var acc (first values))

    # Apply operators sequentially until we exceed sum or finish
    (eachk i ops
      (set acc ((ops i) acc (values (inc i))))
      (when (> acc sum) (break)))

    # If we found our target sum, store result and break
    (when (= acc sum)
      (set found ops)
      (break)))

  found)

(test (filter |(find-operators [+ *] ;$) (peg/match grammar input1))
  @[@[190 [10 19]]
    @[3267 [81 40 27]]
    @[292 [11 6 16 20]]])

(defn solve1 [input]
  (def rules (peg/match grammar input))
  (def valid (filter |(find-operators [+ *] ;$) rules))
  (reduce + 0 (map first valid)))


(test (filter |(find-operators ;$) (peg/match grammar input1))
  @[@[190 [10 19]]
    @[3267 [81 40 27]]
    @[292 [11 6 16 20]]])

(test (solve1 input1) 0)
(test (solve1 day7-input) 1153997401072)

(defn conc [a b] (scan-number (string a b)))

(test (conc 10 20) 1020)

(defn solve2 [input]
  (def rules (peg/match grammar input))
  (def valid (filter |(find-operators [+ * conc] ;$) rules))
  (reduce + 0 (map first valid)))


(test (solve2 input1) 11387)
(test (solve2 day7-input) 97902809384118)
