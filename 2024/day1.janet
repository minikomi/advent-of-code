(import ./util)
(use judge)

(def day1-input (util/read-file "resources/day1.txt"))

(def input1 `3   4
4   3
2   5
1   3
3   9
3   3`)

(def grammar-pt1
  (peg/compile ~{:line
                 (group (*
                          (/ (<- (some :d)) ,scan-number)
                          (some :s)
                          (/ (<- (some :d)) ,scan-number)
                          (? "\n")))
                 :main (some :line)}))

(defn parse [s] (peg/match grammar-pt1 s))

(test (parse input1)
  @[@[3 4]
    @[4 3]
    @[2 5]
    @[1 3]
    @[3 9]
    @[3 3]])

(defn solve1 (lines)
  (->> lines
      (util/transpose)
       (map sort)
       (apply map (fn [a b] (math/abs (- a b))))
       (reduce + 0)))

(test (solve1 (parse input1)) 11)
(test (solve1 (peg/match grammar-pt1 day1-input)) 2970687)

(defn solve2 [lines]
  (let [[l1 l2] (util/transpose lines)
        freqs (frequencies l2)
        ns (map (fn [n] (* (get freqs n 0) n)) l1)]
    (apply + ns)))

(test (solve2 (parse input1)) 31)
(test (solve2 (parse day1-input)) 23963899)
