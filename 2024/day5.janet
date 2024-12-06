(import ./util)
(use judge)

(def day5-input (util/read-file "resources/day5.txt"))

(def input1 `47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47`)

(defn build-order-map [orderings]
  (util/loopv [:let [t @{}]
               [before after] :in orderings]
              (update t before (util/fnil array/push @[]) after)))

(def grammar-pt1 (peg/compile ~{:order-line (group (* (number :d+) "|" (number :d+) "\n"))
                                :page-list (group (* (some (* (number :d+) (? ","))) (? "\n")))
                                :main (*
                                        (group (some :order-line))
                                        :s+
                                        (group (some :page-list)))}))

(defn parse [input]
  (let [[orderings pagelists] (peg/match grammar-pt1 input)]
    @{:order-map (build-order-map orderings) :pagelists pagelists}))

(test (parse input1)
      @{:order-map @{29 @[13]
                     47 @[53 13 61 29]
                     53 @[29 13]
                     61 @[13 53 29]
                     75 @[29 53 47 61 13]
                     97 @[13 61 47 29 53 75]}
        :pagelists [@[75 47 61 53 29]
                    @[97 61 53 29 13]
                    @[75 29 13]
                    @[75 97 47 61 53]
                    @[61 13 29]
                    @[97 13 75 29 47]]})

(defn is-pagelist-correct [order-map pagelist]
  (var ok true)
  (loop [i :range [0 (length pagelist)]
         :until (not ok)
         :let [current (get pagelist i)
               should-be-afters (get order-map current)]
         :when should-be-afters
         before :in (array/slice pagelist 0 i)]
    (set ok (nil? (index-of before should-be-afters))))
  ok)

(test (is-pagelist-correct @{2 [1]} [1 2 3]) false)

(defn get-middle-value [arr]
  (get arr (math/floor (/ (length arr) 2))))

(defn solve1 [text-input]
  (def parsed (parse text-input))
  (->> (parsed :pagelists)
       (filter |(is-pagelist-correct (parsed :order-map) $))
       (map get-middle-value)
       (reduce + 0)))

(test (solve1 input1) 143)
(test (solve1 day5-input) 6949)

# Part 2 -------------------------

(defn sort-by-order-rules [order-map pagelist]
  (sort pagelist
        (fn [a b]
          (let [after-a (get order-map a @[])]
            (not (nil? (index-of b after-a)))))))

(test (sort-by-order-rules ((parse input1) :order-map) @[75 97 47 61 53]) @[97 75 47 61 53])


(defn solve2 [text-input]
  (def parsed (parse text-input))
  (->> (parsed :pagelists)
       (filter |(not (is-pagelist-correct (parsed :order-map) $)))
       (map |(sort-by-order-rules (parsed :order-map) $))
       (map get-middle-value)
       (reduce + 0)))

(test (solve2 input1) 123)
(test (solve2 day5-input) 4145)
