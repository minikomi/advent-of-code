(import ./util)
(use judge)

(def day4-input (util/read-file "resources/day4.txt"))

# Part 1 -----------------------------------------

(def input1 `..X...
.SAMX.
.A..A.
XMAS.S
.X....`)

(def grammar-pt1 (peg/compile ~{:pos (/ (* (line) (column)) ,tuple)
                                :xmas (* :pos (<- (+ "X" "M" "A" "S")))
                                :main (some (+ :xmas 1))}))

(defn parse [s]
  (table ;(peg/match grammar-pt1 s)))

(test (parse input1)
      @{[1 3] "X"
        [2 2] "S"
        [2 3] "A"
        [2 4] "M"
        [2 5] "X"
        [3 2] "A"
        [3 5] "A"
        [4 1] "X"
        [4 2] "M"
        [4 3] "A"
        [4 4] "S"
        [4 6] "S"
        [5 2] "X"})

(defn get-xmas-starts [mtx]
  (->> (pairs mtx)
       (filter (fn [[_ v]] (= "X" v)))
       (map first)))

(test (get-xmas-starts (parse input1)) @[[1 3] [4 1] [5 2] [2 5]])

(def next-letters @{"X" "M" "M" "A" "A" "S" "S" :end})

(defn check-xmas [mtx [y x]]
  (var found 0)
  (loop [[dy dx] :in [[-1 -1] [-1 0] [-1 1]
                      [0 -1] [0 1]
                      [1 -1] [1 0] [1 1]]
         :let [expect (map (fn [[l v]]
                             [l [(+ y (* dy v)) (+ x (* dx v))]])
                           [["M" 1] ["A" 2] ["S" 3]])]
         :when (all (fn [[l pos]]
                      (= l (get mtx pos)))
                    expect)]
    (++ found))
  found)

(deftest "check-xmas"
  (test (check-xmas (parse input1) [1 3])
        1))

(defn solve1 [str-input]
  (def mtx (parse str-input))
  (def xs (get-xmas-starts mtx))
  (+ ;(map |(check-xmas mtx $) xs)))

(def input2 `MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX`)

(test (solve1 input2) 18)
(test (solve1 day4-input) 2562)

# Part 2 -----------------------------------------

(defn get-x-mas-starts [mtx]
  (->> (pairs mtx)
       (filter (fn [[_ v]] (= "A" v)))
       (map first)))


(defn check-x-mas [mtx [y x]]
  (def letters
    (map
      (fn [[dy dx]] (get mtx [(+ y dy) (+ x dx)]))
      [[-1 -1] [-1 1] [1 -1] [1 1]]))
  (match letters
    ["M" "M"
     "S" "S"] true
    ["M" "S"
     "M" "S"] true
    ["S" "M"
     "S" "M"] true
    ["S" "S"
     "M" "M"] true
    _ false))

(test (check-x-mas (parse `.M.S......
..A..MSMS.
.M.S.MAA..
..A.ASMSM.
.M.S.M....
..........
S.S.S.S.S.
.A.A.A.A..
M.M.M.M.M.
..........`) [2 3])
      true)

(defn solve2 [input-str]
  (def mtx (parse input-str))
  (def x-mas-starts (get-x-mas-starts mtx))
  (length (filter |(check-x-mas mtx $) x-mas-starts)))

(test (solve2 input2) 9)
(test (solve2 day4-input) 1902)
