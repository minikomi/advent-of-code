(import ./util :reload-all)
(use judge)

(defn parse [input] (peg/match ~(some (* (number :d+) (? :s+)))
                               input))

(def input1 (parse `0 1 10 99 999`))

(defn num-digits [n]
  (if (zero? n)
    1
    (math/ceil (math/log10 (+ n 1)))))

(defn consume
  "Consume `s` into a new array."
  [s]
  (seq [v :in s] v))

(def cache @{})

(defn step-stones [stones]
    (if-let [v (cache stones)]
        v
        (let [v (consume
                (coro (loop [s :in stones]
                        (cond
                            (zero? s) (yield 1)
                            (even? (num-digits s)) (let [digits (num-digits s)
                                                         power (math/pow 10 (/ digits 2))]
                                                     (yield (math/floor (/ s power)))
                                                     (yield (mod s power)))
                            :else (yield (* s 2024))))))]
        (put cache stones v)
        v)))

(test (step-stones input1)
  @[1 2024 1 0 9 9 2021976])

(num-digits 253000)

(test
(do
(var stones @[125 17])
(seq [_ :range [0 5]]
(set stones (step-stones stones))))
  @[@[253000 1 7]
    @[253 0 2024 14168]
    @[512072 1 20 24 28676032]
    @[512 72 2024 2 0 2 4 2867 6032]
    @[1036288 7 2 20 24 4048 1 4048 8096 28 67 60 32]])

(def day11-input (parse "7725 185 2 132869 0 1840437 62 26310"))

(test (length (reduce (fn [x _] (step-stones x)) [125 17] (range 25))) 55312)
(test (length (reduce (fn [x _] (step-stones x)) day11-input (range 25))) 233050)

# part 2 --------------------

(def memory @{})

(defn solve2 [stone blinks]
  (or (get memory [stone blinks])
   (cond
     # Base case: no more blinks
    (zero? blinks)
    1

     # If stone is 0, move to 1
    (zero? stone)
    (let [val (solve 1 (dec blinks))]
      (put memory [stone blinks] val)
      val)

     # If stone has even digits, split it
    (even? (num-length stone))
    (let [[left right] (split-number stone)
          val (+ (solve left (dec blinks))
                 (solve right (dec blinks)))]
      (put memory [stone blinks] val)
      val)

     # Otherwise multiply by 2024
    (let [val (solve (* stone 2024) (dec blinks))]
      (put memory [stone blinks] val)
      val))))

(+ ;(map |(solve2 $ 75) day11-input))
