(use judge)
(import ./util :fresh true)

(def input1
  `Time:      7  15   30
Distance:  9  40  200`)

(peg/match ~(group (some (* :s+ (number :d+)))) "   1  2   33  4")

(def parser (peg/compile ~{:number-group (some (* :s+ (number :d+)))
                           :line (group (* (+ "Time" "Distance") ":" :number-group (? "\n")))
                           :main (some :line)}))


(peg/match parser input1)

(defn wins [length record]
  (values
    (generate [i :in (map inc (range length))
               :let [distance (* (- length i) i)]
               :when (< record distance)]
      [i distance])))

(defn solve [input]
  (->> (map (comp length wins) ;(peg/match parser input))
       (apply *)))

(test (solve input1) 288)

(def day6-input (util/read-file "./resources/day6.txt"))

(solve day6-input)


(defn solve2 [input]
  (let [[len rec]
        (peg/match ~{:numbers (/ (some (* :s+ (<- :d+))) ,(comp scan-number string))
                     :line (* (+ "Time" "Distance") ":" :numbers (? "\n"))
                     :main (some :line)} input)]
    (length (wins len rec))))

(test (solve2 input1) 71503)
