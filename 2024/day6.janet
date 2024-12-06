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

(defn step [pos-table {:pos [y x] :current-dir current-dir :visited visited}]
  (let [next-visited (put-in visited [[y x] current-dir] true)
        [dy dx] (get directions current-dir current-dir)
        next-pos [(+ y dy) (+ x dx)]
        next-val (get pos-table next-pos)]
      (cond
        (nil? next-val) {:pos next-pos :current-dir :finish :visited next-visited}
        (or (= next-val "#")
            (= next-val "O"))
            {:pos [y x] :current-dir (get turn-map current-dir) :visited next-visited}
        (not (nil? (get-in visited [next-pos current-dir]))) {:pos [y x] :current-dir :loop :visited next-visited}
        :else {:pos next-pos :current-dir current-dir :visited next-visited})))

(test (let [pos-table (parse input1)
            [pos current-dir] (find-start pos-table)]
           (step pos-table {:pos pos :current-dir current-dir :visited @{}}))
  {:current-dir "^"
   :pos [6 5]
   :visited @{[7 5] @{"^" true}}})

(defn solve1 [string-input]
  (var state nil)
  (let [pos-table (parse string-input)
        [pos current-dir] (find-start pos-table)]
    (set state {:pos pos :current-dir current-dir :visited @{pos @{current-dir true}}})
    (loop [new-state :iterate (step pos-table state)
           :until (= (get state :current-dir) :finish)]
      (set state new-state))
    state))


# Note: we finish outside so dec
(test (dec (length ((solve1 input1) :visited))) 41)
(test (dec (length ((solve1 day6-input) :visited))) 4433)

(defn does-it-loop? [pos-table [pos current-dir]]
  (var state {:pos pos :current-dir current-dir :visited (tabseq [k :in (keys (parse input1))] k @{})})
  (loop [new-state :iterate (step pos-table state)
         :until (or (= (get state :current-dir) :finish)
                    (= (get state :current-dir) :loop))]
    (set state new-state))
  state)

(test (does-it-loop? input1 [7 4])
  {:current-dir :loop
   :pos [7 5]
   :visited @{[2 5] @{">" true "^" true}
              [2 6] @{">" true}
              [2 7] @{">" true}
              [2 8] @{">" true}
              [2 9] @{">" true "v" true}
              [3 5] @{"^" true}
              [3 9] @{"v" true}
              [4 5] @{"^" true}
              [4 9] @{"v" true}
              [5 5] @{"^" true}
              [5 9] @{"v" true}
              [6 5] @{"^" true}
              [6 9] @{"v" true}
              [7 5] @{"<" true "^" true :loop true}
              [7 6] @{"<" true}
              [7 7] @{"<" true}
              [7 8] @{"<" true}
              [7 9] @{"<" true "v" true}}})

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
        (when (and (= "." (get pos-table obs-pos))
                   (not= start-pos obs-pos))
          (let [final-state (does-it-loop? (put (table/clone pos-table) obs-pos "O") [start-pos start-dir])]
            (when (= :loop (final-state :current-dir))
              (++ looped)))))
      (length obs-positions))
    looped))

(test (solve2 input1) 6)
(test (solve2 day6-input) 1516)

(defn main [& args] (pp (solve2 day6-input)))
