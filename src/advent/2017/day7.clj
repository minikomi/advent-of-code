(ns advent.2017.day7
  (require [clojure.string :as s]
           [clojure.java.io :as io]
           [clojure.walk :as w]))

(defn parse-weight [weight-str]
  (Integer/parseInt (clojure.string/replace weight-str #"[()]" "")))

(defn parse-line [line]
  (let [[name weight-str & [arrow & supported]] (s/split line #" ")
        w (parse-weight weight-str)
        supported (when (= "->" arrow)
                    (mapv #(s/replace % #"[, ]" "") supported))]
    {:name name
     :weight w
     :supporting supported}))


(def test-input
  (map parse-line
       (s/split-lines
        "pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)")))

(def input
  (map parse-line (s/split-lines (slurp (io/resource "day7.txt")))))

(def input2
  (map parse-line (s/split-lines (slurp (io/resource "day72.txt")))))

(defn input->map [input]
  (into {} (map #(vector (:name %) %)) input))

;; part 1

(defn solve1 [input]
  (let [supporting (filter :supporting input)
        all-supported (set (mapcat :supporting supporting))]
    (first (filter #(not (all-supported %)) (map :name supporting)))))

(comment (solve1 input))

;; part 2

(defn walk-tally [name-map root]
  (tree-seq
   #(map :supporting %)
   #(->> % :supporting
         (map name-map)
         (map (fn [p]
                (assoc p
                       :parent (:name %)
                       :tally
                       (apply +
                              (map :weight
                                   (walk-tally name-map (:name p))))))))
   (let [root-guy (name-map root)]
     (assoc root-guy :parent nil))))


(defn solve2 [input]
  (let [head (solve1 input)
        name-map (input->map input)
        tallied (walk-tally name-map head)
        unbalanced-discs
        (->> tallied
             (group-by :parent)
             (filter #(apply not= (map :tally (second %)))))
        [unbalanced-parent unbalanced-children]
        (apply min-key
               #(-> % second first :tally)
               unbalanced-discs)
        [correct-weight _]
        (apply max-key
               #(-> % second count)
               (group-by :tally unbalanced-children))
        [incorrect-weight [unbalanced-node]]
        (apply min-key
               #(-> % second count)
               (group-by :tally unbalanced-children))]
    {:correct-weight correct-weight
     :incorrect-weight incorrect-weight
     :unbalanced-node (select-keys unbalanced-node [:name :weight])
     :corrected-weight
     (+ (:weight unbalanced-node)
        (- correct-weight incorrect-weight))}))
