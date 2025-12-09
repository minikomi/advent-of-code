(import ./util)
(use judge)

(def day2-input (util/read-file "resources/day2.txt"))

(def input1 `7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9`)

(def grammar-pt1
  (peg/compile ~{:line (* (group (some (* (any " ") (number :d+))))
                          (? "\n"))
                 :main (some :line)}))

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
              b (get row (inc i))]]
    @[a b]))

(test (partition-pairs @[1 2 3 4]) @[@[1 2] @[2 3] @[3 4]])

(defn grade [row]
  (let [[a b & rest] row
        compare (if (< a b) < >)
        pairs (partition-pairs row)]
    (seq [[a b] :in pairs]
      (and (compare a b)
           (> 4 (math/abs (- a b)))))))

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
       length))

(test (solve1 (parse input1)) 2)
(test (solve1 (parse day2-input)) 332)

(defn remove-idx [row idx]
  (array/concat (array/slice row 0 idx)
                (array/slice row (inc idx))))

(test (remove-idx @[1 2 3 4] 1) @[1 3 4])

(defn grade2 [row]
  (some |(all true? (grade $))
        (array/concat
          @[row]
          (map (fn [i] (remove-idx row i)) (range (length row))))))


(deftest "grade2"
  (test (grade2 @[1 5])
        true)

  (test (grade2 @[7 6 4 2 1])
        true)

  (test (grade2 @[1 3 2 4 5])
        true)

  (test (grade2 @[1 2 7 8 9])
        nil))

(test (map grade2 (parse input1))
      @[true nil nil true true true])


(defn solve2 [rows]
  (->> rows
       (filter grade2)
       length))

(test (solve2 (parse input1)) 4)
(test (solve2 (parse day2-input)) 398)
