(import ./util :fresh true)
(use judge)

(def day9-input (util/read-file "./resources/day9.txt"))

(def input1 `0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45`)

(def parse ~(some (* (group (some (* (number (* (? "-") :d+)) (? " "))))
                     (? "\n"))))

(def line1 (peg/match parse input1))

(test (peg/match parse "-1 -2 -3") @[@[-1 -2 -3]])

(defn differences [line]
  (seq [i :range [0 (dec (length line))]]
    (- (line (inc i)) (line i))))

(test (differences line1) @[3 3 3 3 3])

(defn diff-seq [start-v]
  (var current start-v)
  (var last-v [999])
  (seq [v :in (coro
                (while (not (all zero? last-v))
                  (yield current)
                  (set last-v current)
                  (set current (differences current))))]
    v))

(defn fill-ends [diffs]
  (var current 0)
  (seq [i :range [0 (dec (length diffs))]]
    (set current (+ (last (diffs (- (length diffs) 2 i)))
                    current))))

(defn solve [input]
  (->> (peg/match parse input)
       (map (comp fill-ends diff-seq))
       (map last)
       sum))

(test (solve input1) 114)
(test (solve day9-input) 1887980197)

(defn fill-starts [diffs]
  (var current 0)
  (seq [i :range [0 (dec (length diffs))]]
    (set current (- (first (diffs (- (length diffs) 2 i)))
                    current))))

(test (fill-starts (diff-seq (first (peg/match parse "10 13 16 21 30 45"))))
      @[2 -2 5 5])

(defn solve2 [input]
  (->> (peg/match parse input)
       (map (comp fill-starts diff-seq))
       (map last)
       sum))

(test (solve2 input1) 2)
(test (solve2 day9-input) 990)
