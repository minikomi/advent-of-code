(import ./util :fresh true)
(use judge)

(def input1 `467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..`)

(def parser (peg/compile
              ~{:matrix-pos (group (* (line) (column)))
                :num (constant :num)
                :sym (constant :sym)
                :main (some
                        (+ "."
                           :s
                           (group (* :num :matrix-pos (<- :d+)))
                           (group (* :sym :matrix-pos (<- 1)))))}))

(defn add-number-to-map [map [l c] num-str]
  (loop [i :range [0 (length num-str)]]
    (let [col (+ c i)]
      (put map [l col] [num-str [l c]]))))

(defn gen-num-map [parsed]
  (def collect @{})
  (loop [[kind pos val] :in parsed
         :when (= kind :num)]
    (add-number-to-map collect pos val))
  collect)

(defn extract-parts-numbers [parsed]
  (var num-map (gen-num-map parsed))
  (var seen @{})
  (var collect @[])
  (loop [[kind [l c] val] :in parsed
         :when (= kind :sym)
         y-offset :in [-1 0 1]
         x-offset :in [-1 0 1]
         :when (not= [0 0] [y-offset x-offset])
         :let [pos [(+ l y-offset) (+ c x-offset)]
               entry (get num-map pos)]
         :when entry
         :let [[num-str id] entry]
         :when (not (get seen id))]
    (put seen id true)
    (array/push collect num-str))
  collect)

(def day3-input (util/read-file "./resources/day3.txt"))

(defn solve1 [input]
  (->> (extract-parts-numbers (peg/match parser input))
       # parse all values to numbers
       (map scan-number)
       sum))

(test (solve1 input1) 4361)
(test (solve1 day3-input) 533775)

(defn extract-gears-numbers [parsed]
  (var num-map (gen-num-map parsed))
  (var collect @[])
  (loop [[kind [l c] val] :in parsed
         :when (and (= kind :sym)
                    (= val "*"))
         :let [gear-vals @[]
               seen-val-ids @{}]
         y-offset :in [-1 0 1]
         x-offset :in [-1 0 1]
         :when (not= [0 0] [y-offset x-offset])
         :let [pos [(+ l y-offset) (+ c x-offset)]
               entry (get num-map pos)]]

    (when-let [[num-str id] entry]
      (when (not (get seen-val-ids id))
        (array/push gear-vals num-str)
        (put seen-val-ids id true)))
    (when (and (= [1 1] [y-offset x-offset])
               (= 2 (length gear-vals)))
      (array/push collect gear-vals)))
  collect)

(test (extract-gears-numbers (peg/match parser input1))
      @[@["467" "35"] @["755" "598"]])

(defn solve2 [input]
  (->> (extract-gears-numbers (peg/match parser input))
       (map |(product (map scan-number $0)))
       sum))

(test (solve2 input1) 467835)
(test (solve2 day3-input) 78236071)
