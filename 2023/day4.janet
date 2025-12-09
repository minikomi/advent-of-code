(import ./util :fresh true)
(use judge)

(def input1 (string/split "\n"
                          `Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11`))

(def parser
  ~{:card (* "Card" :s+ (number :d+))
    :numbers (group (some (* (number :d+) (any " "))))
    :main (* :card ":" :s+ :numbers "|" :s+ :numbers)})

(defn my-win-vals [[id winning-vals card-vals]]
  (def win-set (reduce |(put $0 $1 true) @{} winning-vals))
  (filter |(get win-set $0) card-vals))

(defn solve1 [input]
  (var total 0)
  (loop [line :in input
         :let [parsed (peg/match parser line)
               winning-values (my-win-vals parsed)
               line-score (if (empty? winning-values)
                            0
                            (math/pow 2 (dec (length winning-values))))]]
    (set total (+ total line-score)))
  total)

(def day4-input (util/read-lines "./resources/day4.txt"))

(test (solve1 input1) 13)
(test (solve1 day4-input) 26218)

(defn solve2 [input]
  (let [collect (tabseq [i :range [1 (inc (length input))]] i 1)]
    (loop [line :in input
           :let [parsed (peg/match parser line)
                 card (first parsed)
                 winning-vals (my-win-vals parsed)]
           delta :range [0 (length winning-vals)]
           :let [updated (+ card delta 1)]]
      (update collect updated + (get collect card 1)))
    (sum (values collect))))

(test (solve2 input1) 30)
(test (solve2 day4-input) 9997537)
