(ns day7
  (:require
   [clojure.string :as str]
   [criterium.core :as c]
   [dom-top.core :refer [loopr]]
   [instaparse.core :as insta]))

(def input1 ".......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............")


(defn parse [s]
  (let [grammar "
    main        = field-line (<'\n'> field-line)* 
    field-line  = token+
    token       = (start | split | blank)
    start       = 'S'
    split       = '^'
    blank       = '.'
"
        parser (insta/parser grammar)
        parse-tree (parser s)
        with-metadata (insta/add-line-and-column-info-to-metadata s parse-tree)]
    (insta/transform
     {:main  (fn [start & ns] {:start (first (keys start)) :rows (vec (remove empty? ns))})
      :token first
      :field-line (fn [& vs]
                    (into {}
                          (keep-indexed
                           (fn [idx v] (when (not= :blank v) [idx v])) vs)))}
     parse-tree)))

(comment
  (parse input1)
  )

(defn step1 [[current-beam-indexes beam-split-count] row]
  (loopr
      [new-beam-indexes #{} split-count beam-split-count]
      [beam-index current-beam-indexes]
      (if (row beam-index)
        (recur (conj new-beam-indexes (dec beam-index) (inc beam-index))
               (inc split-count))
        (recur (conj new-beam-indexes beam-index) split-count))))

(defn solve1 [{:keys [start rows]}]
  (reduce step1 [#{start} 0] rows))

(comment 
  (solve1 (parse input1)))

(defn step2 [path-indexes row]
  (loopr
      [new-path-indexes {}]
      [[idx n] (sort-by first path-indexes)]
      (if (row idx)
        (recur (-> new-path-indexes
                   (update (dec idx) (fnil + 0) n)
                   (update (inc idx) (fnil + 0) n)))
        (recur (update new-path-indexes idx (fnil + 0) n)))))

(defn solve2 [{:keys [start rows]}]
  (reduce step2 {start 1} rows))

(comment (reduce + (vals (solve2 (parse input1)))))

(comment (reduce + (vals (solve2 (parse (str/trim (slurp "./inputs/day7.txt")))))))




