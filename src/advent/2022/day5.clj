(ns advent.2022.day5
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def input (->> (io/resource "2022/day5-input1.txt")
                slurp))


(def test-input "    [D]
[N] [C]
[Z] [M] [P]
 1   2   3

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2")


(defn parse-input [s]
  (let [lines (str/split-lines s)
        start-state-lines (take-while #(not (str/blank? %)) lines)
        move-lines (drop (inc (count start-state-lines)) lines)]
    [start-state-lines move-lines]))


(comment
  (parse-input test-input)
  (map second (partition-all 4 "[a] [b] [c]"))
  ;;
  )

(defn parse-start-state [lns]
  (let [ns (str/split (str/trim (last lns)) #" +")
        box-lines (rest (reverse lns))
        acc (into {} (map vector ns (repeat [])))]
    (reduce
     (fn [acc ln]
       (let [vs (into {} (map vector ns (map second (partition-all 4 ln))))]
         (merge-with (fn [a b] (if (not= \space b) (conj a b) a)) acc vs)))
     acc
     box-lines
     )))

(comment
  (parse-start-state (first (parse-input test-input)))
  ;;
  )

(defn parse-movements [lns]
  (->> lns
       (map #(str/split % #" "))
       (map (fn [[_ n _ from _ to]]
              [(Integer/parseInt n) from to]))))

(comment
  (parse-movements (second (parse-input test-input)))
  ;;
  )

(defn do-move [state [n from to]]
  (reduce
   (fn [state _]
     (-> state
         (update to conj (peek (state from)))
         (update from pop)))
   state
   (range n)))

(comment
  (do-move {"a" [1 2 3] "b" []}
           [2 "a" "b"])
  )

(defn solve [input]
  (let [[state-lines mv-lines] (parse-input input)
        start-state (parse-start-state state-lines)
        movements (parse-movements mv-lines)
        final-state (reduce do-move start-state movements)
        sorted (sort-by first final-state)
        top-crates (map (comp last second) sorted)
        ]
    (apply str top-crates)))

(defn do-move2 [state [n from to]]
  (println state [n from to])
  (let [froms (state from)
        froms-n (count froms)]
    (-> state
        (update to into (subvec froms (- froms-n n) froms-n))
        (assoc from (subvec froms 0 (- froms-n n)))

        )

    ))

(comment
  (do-move2 {"a" [1 2 3] "b" []}
            [2 "a" "b"])
  )


(defn solve2 [input]
  (let [[state-lines mv-lines] (parse-input input)
        start-state (parse-start-state state-lines)
        movements (parse-movements mv-lines)
        final-state (reduce do-move2 start-state movements)
        sorted (sort-by first final-state)
        top-crates (map (comp last second) sorted)
        ]
    (apply str top-crates)))

(comment
  (assert (= (solve2 test-input) "MCD"))
  (solve2 input)
  ;;
  )
