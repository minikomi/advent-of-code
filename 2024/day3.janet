(import ./util)
(use judge)

(def day3-input (util/read-file "resources/day3.txt"))

# Part 1 -----------------------------------------

(def input1 `xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))`)

(def grammar-pt1 (peg/compile ~{:mul (/ (* "mul(" (number :d+) "," (number :d+) ")") ,*)
                                :main (some (+ :mul 1))}))

(defn parse [s] (peg/match grammar-pt1 s))

(test (parse input1) @[8 25 88 40])

(defn solve1 [input-str]
  (+ ;(parse input-str)))

(test (solve1 input1) 161)
(test (solve1 day3-input) 191183308)

# Part 2 -----------------------------------------

(def grammar-pt2 (peg/compile ~{:mul (/ (* "mul(" (number :d+) "," (number :d+) ")") ,*)
                                :do (/ "do()" :do)
                                :dont (/ "don't()" :dont)
                                :main (some (+ :mul :do :dont 1))}))

(def input2 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

(defn parse2 [s] (peg/match grammar-pt2 s))

(test (parse2 input2)
      @[8 :dont 25 88 :do 40])

(defn solve2 [input-str]
  (reduce (fn [{:state state :acc acc} v]
            (cond
              (= v :do) {:state :do :acc acc}
              (= v :dont) {:state v :acc acc}
              (= state :do) {:state state :acc (+ acc v)}
              :else {:state :dont :acc acc}))
          {:state :do :acc 0}
          (parse2 input-str)))

(test (solve2 input2) {:acc 48 :state :do})
(test (solve2 day3-input) {:acc 92082041 :state :do})
