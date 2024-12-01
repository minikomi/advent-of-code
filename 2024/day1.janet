(import ./util)

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

(peg/match grammar-pt1 input1)

(apply map array @[@[1 2] @[3 4]])

(defn solve1 (lines)
  (let [[l1 l2] (apply map array lines)]
    (apply +
           (map (fn [a b] (math/abs (- a b)))
                (sort l1) (sort l2)))))

(comment
  (solve1 input1)
  (solve1 (peg/match grammar-pt1 day1-input)))


(defn solve2 [lines]
  (let [[l1 l2] (apply map array lines)
        freqs (frequencies l2)
        ns (map (fn [n] (* (get freqs n 0) n)) l1)]
    (apply + ns)))

(solve2 (peg/match grammar-pt1 input1))

(solve2 (peg/match grammar-pt1 day1-input))

(def input2 (string/split
              "\n"
              `two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen`))

(def grammar-pt1
  (peg/compile ~{:main (some (+ (<- :d) 1))}))

(defn pt1 [lines]
  (reduce (fn [total s]
            (def digits (peg/match grammar-pt1 s))
            (+ total
               (scan-number
                 (string (first digits) (last digits)))))
          0
          lines))

(def grammar-pt2
  (peg/compile
    ~{:eng-number (+
                    (/ (* (look -1 "o") "ne") "1")
                    (/ (* (look -1 "t") "wo") "2")
                    (/ (* (look -1 "t") "hree") "3")
                    (/ (* (look -1 "f") "our") "4")
                    (/ (* (look -1 "f") "ive") "5")
                    (/ (* (look -1 "s") "ix") "6")
                    (/ (* (look -1 "s") "even") "7")
                    (/ (* (look -1 "e") "ight") "8")
                    (/ (* (look -1 "n") "ine") "9"))
      :main (some (+ (<- :d)
                     :eng-number
                     1))}))

(defn pt2 [lines]
  (reduce (fn [total s]
            (def digits (peg/match grammar-pt2 s))
            (+ total
               (scan-number
                 (string (first digits) (last digits)))))
          0
          lines))

(comment
  (pp (pt1 day1-input1)))

(comment
  (pp (pt2 day1-input)))
