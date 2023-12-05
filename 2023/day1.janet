(import ./util)

(def day1-input (util/read-lines "resources/day1.txt"))

(def input1 (string/split "\n"
                          `1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet`))

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
