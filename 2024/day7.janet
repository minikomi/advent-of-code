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
292: 11 6 16 20
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20
`)

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
  # For n items chosen from m options, we iterate from 0 to m^n
  (def m (length xs))
  (def limit (math/pow m n))
  
  (generate [i :range [0 limit]]
    (def result @[])
    (var num i)
    # Convert number to base-m representation
    (for j 0 n
      (def idx (% num m))
      (array/push result (get xs idx))
      (set num (math/floor (/ num m))))
    result))

(test (seq [c :in (combinations-gen [1 2] 3)] c)
  @[[1 1 1]
    [1 1 2]
    [1 2 1]
    [1 2 2]
    [2 1 1]
    [2 1 2]
    [2 2 1]
    [2 2 2]])

(defn find-operators [operators [sum values]]
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
  (def valid (filter |(find-operators [+ *] $) rules))
  (reduce + 0 (map first valid)))


(test (filter |(find-operators ;$) (peg/match grammar input1))
  @[@[190 [10 19]]
    @[3267 [81 40 27]]
    @[292 [11 6 16 20]]])

(test (solve1 input1) 14426)
(test (solve1 day7-input) 1153997401072)

(defn digit-count [n]
  (if (zero? n)
    1
    (math/floor (+ 1 (math/log10 n)))))

(defn conc [a b]
  (+ (* a (math/pow 10 (digit-count b))) b))

(test (conc 10 20) 1020)

(defn solve2 [input]
  (def rules (peg/match grammar input))
  (def valid (filter |(find-operators [+ * conc] $) rules))
  (reduce + 0 (map first valid)))

(defn dfs-find-operators
  "Recursively find operators that combine numbers to reach target sum.
   Returns nil if no solution found."
  [curr-sum target-sum operators remaining-nums prev-ops]
  (cond
    # Base case: exceeded target - abort this branch
    (> curr-sum target-sum)
    nil

    # Base case: no more numbers and we hit target
    (and (empty? remaining-nums) (= curr-sum target-sum))
    prev-ops

    # Base case: no more numbers but didn't hit target
    (empty? remaining-nums)
    nil

    # Recursive case: try each operator
    (let [next-num (first remaining-nums)
          rest-nums (array/slice remaining-nums 1)]
      (var found nil)
      (each op operators
        (when (nil? found)  # Only continue searching if no solution found
          (let [next-sum (op curr-sum next-num)
                next-ops (tuple/join prev-ops [op])]
            (set found (dfs-find-operators
                        next-sum
                        target-sum
                        operators
                        rest-nums
                        next-ops)))))
      found)))

(defn solve2-dfs [text-input]
  (def rules (peg/match grammar text-input))
  (pp (first rules))
  (def valid (filter |(dfs-find-operators 0 (first $) [+ * conc] ($ 1) @[]) rules))
  ( + ;(map first valid)))


(test (solve2 input1) 44978)
(test (solve2 day7-input) 97902809384118)

(defn main [&] (pp (solve2-dfs day7-input)))
