(import ./util :reload-all)
(use judge)

(def input1 `............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............`)

(def day8-input (util/read-file "resources/day8.txt"))

(defn parse [input]
  (let [all (first (peg/match util/mtx-grammar input))]
   @{:map (tabseq [p :in (pairs all) :when (not= (p 1) ".")] (p 0) (p 1))
     :size (last (sort (keys all)))}))

(test (parse input1)
  @{:map @{[2 9] "0"
           [3 6] "0"
           [4 8] "0"
           [5 5] "0"
           [6 7] "A"
           [9 9] "A"
           [10 10] "A"}
    :size [12 12]})

(defn print-map [antenna-map]
  (seq [row :range [1 (inc (get-in antenna-map [:size 0]))]]
        (string/join (seq [col :range [1 (inc (get-in antenna-map [:size 1]))]
        :let [char (get-in antenna-map [:map [row col]] ".")]]
     char))))

(test (print-map (parse input1))
  @["............"
    "........0..."
    ".....0......"
    ".......0...."
    "....0......."
    "......A....."
    "............"
    "............"
    "........A..."
    ".........A.."
    "............"
    "............"])

(defn dist-v [v1 v2]
  (let [d (util/sub-v v1 v2)]
    (+ (math/abs (d 0)) (math/abs (d 1)))))

(defn in-map? [antenna-map pos]
  (and
    (>= (get-in antenna-map [:size 0]) (pos 0))
    (>= (get-in antenna-map [:size 1]) (pos 1))
    (< 0 (pos 0))
    (< 0 (pos 1))))

(defn get-antinodes [antenna-map]
  (seq [[pos1 v1] :in (pairs (antenna-map :map))
        [pos2 v2] :in (pairs (antenna-map :map))
        :when (and (= v1 v2) (not= pos1 pos2))
        :let [new-node (util/add-v pos1 (util/sub-v pos1 pos2))]
        :when (in-map? antenna-map new-node)]
     [new-node v1]))


(test (get-antinodes (parse input1))
  @[[[7 4] "0"]
    [[8 1] "0"]
    [[6 2] "0"]
    [[1 7] "0"]
    [[4 3] "0"]
    [[2 4] "0"]
    [[1 12] "0"]
    [[3 11] "0"]
    [[5 10] "0"]
    [[6 7] "0"]
    [[3 5] "A"]
    [[2 4] "A"]
    [[12 11] "A"]
    [[8 8] "A"]
    [[11 11] "A"]])

(defn solve1 [input]
  (def new-nodes (get-antinodes (parse input)))
  (def seen @{})
  (each n new-nodes (put seen (first n) true))
  (length seen))

(test (solve1 input1) 14)
(test (solve1 day8-input) 348)


(defn get-antinodes2 [antenna-map]
  (loop [[pos1 v1] :in (pairs (antenna-map :map))
         [pos2 v2] :in (pairs (antenna-map :map))
         :when (and (= v1 v2) (not= pos1 pos2))
         :before (var dist (util/sub-v pos1 pos2))
         :before (var p pos1)
         new-node :iterate (do (set p (util/add-v p dist)) p)
         :while (in-map? antenna-map new-node)]
   (put antenna-map new-node "#"))
  antenna-map)

(def input2 `T.........
...T......
.T........
..........
..........
..........
..........
..........
..........
..........`)

(test (get-antinodes2 (parse input2))
  @{[1 6] true
    [3 7] true
    [4 10] true
    [5 3] true
    [7 4] true
    [9 5] true})

(defn solve2 [input]
  (def a-m (parse input))
  (def new-map (get-antinodes2 a-m))
  (length new-map))

(test (let [a-m (parse input1)
            new-nodes (get-antinodes2 a-m)
            merged (update a-m :map (fn [m] (merge m new-nodes)))]
  (print-map merged))
  @["##....#....#"
    ".#.#....0..."
    "..#.#0....#."
    "..##...0...."
    "....#....#.."
    ".#...##....#"
    "...#..#....."
    "#....#.#...."
    "..#.....A..."
    "....#....A.."
    ".#........#."
    "...#......##"])

(test (solve2 input1) 34)
(test (solve2 day8-input) 1221)
