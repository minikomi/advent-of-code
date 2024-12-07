(import ./util :reload-all)
(import spork/ev-utils)
(use judge)

(def input1 `....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...`)

(def day6-input (util/read-file "resources/day6.txt"))

(defn parse [input]
  (first (peg/match util/mtx-grammar input)))


(test (parse input1)
  @{[1 1] "."
    [1 2] "."
    [1 3] "."
    [1 4] "."
    [1 5] "#"
    [1 6] "."
    [1 7] "."
    [1 8] "."
    [1 9] "."
    [1 10] "."
    [2 1] "."
    [2 2] "."
    [2 3] "."
    [2 4] "."
    [2 5] "."
    [2 6] "."
    [2 7] "."
    [2 8] "."
    [2 9] "."
    [2 10] "#"
    [3 1] "."
    [3 2] "."
    [3 3] "."
    [3 4] "."
    [3 5] "."
    [3 6] "."
    [3 7] "."
    [3 8] "."
    [3 9] "."
    [3 10] "."
    [4 1] "."
    [4 2] "."
    [4 3] "#"
    [4 4] "."
    [4 5] "."
    [4 6] "."
    [4 7] "."
    [4 8] "."
    [4 9] "."
    [4 10] "."
    [5 1] "."
    [5 2] "."
    [5 3] "."
    [5 4] "."
    [5 5] "."
    [5 6] "."
    [5 7] "."
    [5 8] "#"
    [5 9] "."
    [5 10] "."
    [6 1] "."
    [6 2] "."
    [6 3] "."
    [6 4] "."
    [6 5] "."
    [6 6] "."
    [6 7] "."
    [6 8] "."
    [6 9] "."
    [6 10] "."
    [7 1] "."
    [7 2] "#"
    [7 3] "."
    [7 4] "."
    [7 5] "^"
    [7 6] "."
    [7 7] "."
    [7 8] "."
    [7 9] "."
    [7 10] "."
    [8 1] "."
    [8 2] "."
    [8 3] "."
    [8 4] "."
    [8 5] "."
    [8 6] "."
    [8 7] "."
    [8 8] "."
    [8 9] "#"
    [8 10] "."
    [9 1] "#"
    [9 2] "."
    [9 3] "."
    [9 4] "."
    [9 5] "."
    [9 6] "."
    [9 7] "."
    [9 8] "."
    [9 9] "."
    [9 10] "."
    [10 1] "."
    [10 2] "."
    [10 3] "."
    [10 4] "."
    [10 5] "."
    [10 6] "."
    [10 7] "#"
    [10 8] "."
    [10 9] "."
    [10 10] "."})

(defn find-start [pos-table]
   (var start nil)
   (util/loopv [[pos v] :in (pairs pos-table)
                :until start]
     (when (and (not= v "#") (not= v ".") (not= v "O"))
       (set start [pos v]))))

(test (find-start (parse input1)) [[7 5] "^"])

(def directions
  {"^" [-1 0]
   ">" [0 1]
   "v" [1 0]
   "<" [0 -1]})

(def turn-map
  {"^" ">"
   ">" "v"
   "v" "<"
   "<" "^"})

(defn run-loop [pos-table start-pos start-dir]
  (var pos start-pos)
  (var current-dir start-dir)
  (var visited @{})
  (while (not (or (= current-dir :finish) (= current-dir :loop)))
    (put-in visited [pos current-dir] true)
    (let [dir (get directions current-dir)
          next-pos [(+ (pos 0) (dir 0)) (+ (pos 1) (dir 1))]
          next-val (get pos-table next-pos)]
      (cond (nil? next-val) (set current-dir :finish)
            (or (= next-val "#")
                (= next-val "O")) (set current-dir (get turn-map current-dir))
            (not (nil? (get-in visited [next-pos current-dir]))) (set current-dir :loop)
            :else (set pos next-pos))))
  {:pos pos
   :visited visited
   :current-dir current-dir})

(defn solve1 [string-input]
  (let [pos-table (parse string-input)
        [start-pos start-dir] (find-start pos-table)]
    (run-loop pos-table start-pos start-dir)))

# Note: we finish outside so dec
(test (length ((solve1 input1) :visited)) 41)
(test (length ((solve1 day6-input) :visited)) 4433)

(defn solve2 [string-input]
  (let [pos-table (parse string-input)
        str-lines (string/split "\n" string-input)
        [ysize xsize] [(length str-lines) (length (first str-lines))]
        [pos current-dir] (find-start pos-table)
        obs-positions (keys ((solve1 string-input) :visited))
        [start-pos start-dir] (find-start pos-table)]
    (var looped 0)
    (ev-utils/pcall
      (fn [n]
        (var obs-pos (get obs-positions n))
        (when (and (= "." (get pos-table obs-pos)) (not= start-pos obs-pos))
          (put pos-table obs-pos "O")
          (let [final-state (run-loop pos-table start-pos start-dir)]
            (when (= :loop (final-state :current-dir)) (++ looped)))
          (put pos-table obs-pos ".")))
      (length obs-positions))
    looped))

(test (solve2 input1) 6)
(test (solve2 day6-input) 1516)

(defn main [& args] (pp (solve2 day6-input)))
