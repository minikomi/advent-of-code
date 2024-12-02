(import ./util)
(use judge)

(def day2-input (util/read-file "resources/day2.txt"))

(def input1 `7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9`)

# recursive grammar for matricies
(def grammar-pt1
  (peg/compile ~{:line (*
                         (group (some (* (any " ") (number :d+))))
                         (? "\n"))
                 :main (some :line)}))

(peg/match grammar-pt1 input1)

(defn parse [s] (peg/match grammar-pt1 s))

(test (parse input1)
  @[@[7 6 4 2 1]
    @[1 2 7 8 9]
    @[9 7 6 2 1]
    @[1 3 2 4 5]
    @[8 6 4 4 1]
    @[1 3 6 7 9]])

(defn partition-pairs [row]
  (seq [i :range [0 (dec (length row))]
         :let [a (get row i)
               b (get row (inc i))
            ]]
    @[a b]))

(test (partition-pairs @[1 2 3 4]) @[@[1 2] @[2 3] @[3 4]])

(defn grade [row]
  (let [[a b & rest] row]
    (let [compare (if (< a b) < >)]
      (seq [[a b] :in (partition-pairs row)]
        (and (compare a b)
             (> 4 (math/abs (- a b))))))))

(test (map grade (parse input1))
  @[@[true true true true]
    @[true false true true]
    @[true true false true]
    @[true false true true]
    @[true true false true]
    @[true true true true]])

(defn solve1 [rows]
  (->> rows
       (map grade)
       (filter |(all true? $))
       length
       ))

(test (solve1 (parse input1)) 2)
(test (solve1 (parse day2-input)) 332)



(defn validate-sequence [nums]
  (when (< (length nums) 3)
    (error "Sequence must have at least 3 numbers"))

  (let [[first second & rest] nums]
    (reduce
      (fn [{:history [prev2 prev1] :chances chances :results results} curr]
        (def prev1-safe?
          (and (> curr prev1)
              (<= 4 (math/abs (- curr prev1)))))

        (def prev2-safe?
          (and (> curr prev2)
              (<= 4 (math/abs (- curr prev2)))))

        (def [new-chances position-valid]
          (cond
            (and (not prev1-safe?) (= 0 chances))
            [0 false]

            (and (not prev1-safe?)
                 (not prev2-safe?)
                 (< 0 chances))
            [(dec chances) false]

            :else
            [chances true]))

        (def next {:history [prev1 curr]
          :chances new-chances
          :results (array/push results position-valid)})
        (pp next)
        next
        )

      {:history [first second]
       :chances 1
       :results @[true true]}
      rest)))

(map pp (map validate-sequence (parse input1)))
