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
  (map parse-line (s/split-lines (slurp (io/resource "2017/day7.txt")))))

(def input2
  (map parse-line (s/split-lines (slurp (io/resource "2017/day72.txt")))))

(def input-deep
  (map parse-line (s/split-lines (slurp (io/resource "2017/day7deep.txt")))))

(def input-bork
  (map parse-line (s/split-lines (slurp (io/resource "2017/day7-bork.txt")))))

(defn input->map [input]
  (into {} (map #(vector (:name %) %)) input))

;; part 1

(defn solve1 [input]
  (let [supporting (filter :supporting input)
        all-supported (set (mapcat :supporting supporting))]
    (first (filter #(not (all-supported %)) (map :name supporting)))))

(comment (solve1 input))

;; part 2

(defn walk-tally [name-map root-node]
  (tree-seq
   #(:supporting %)
   (fn [parent-node]
     (->> (:supporting parent-node)
          (map
           (fn [node-name]
             (let [node (name-map node-name)]
               (assoc node
                      :parent (:name parent-node)
                      :tally (->> (walk-tally name-map node)
                                  (map :weight)
                                  (apply +))))))))
   root-node))

(defn solve2 [input]
  (let [name-map (input->map input)
        head (name-map (solve1 input))
        tallied (walk-tally name-map head)
        unbalanced-discs
        (->> tallied
             (group-by :parent)
             (filter #(apply not= (map :tally (second %)))))
        [unbalanced-parent unbalanced-children]
        (apply min-key
               #(-> % second first :tally)
               unbalanced-discs)
        tally-grouped
        (group-by :tally unbalanced-children)
        [correct-weight _]
        (apply max-key
               #(-> % second count)
               tally-grouped)
        [incorrect-weight [unbalanced-node]]
        (apply min-key
               #(-> % second count)
               tally-grouped)]
    {:correct-weight correct-weight
     :incorrect-weight incorrect-weight
     :unbalanced-node (select-keys unbalanced-node [:name :weight])
     :adujusted-head-weight
     (+ (:weight unbalanced-node)
        (- correct-weight incorrect-weight))}))

;; recur based answer

(defn tally-chain [name-map name]
  (loop [total 0 current-name name stack []]
    (let [supporting (-> (name-map current-name) :supporting)
          new-total (+ total (-> (name-map current-name) :weight))]
      (if (and (empty? stack)
               (nil? supporting)) new-total
          (let [new-stack (into stack supporting)]
            (recur new-total (peek new-stack) (pop new-stack)))))))

(defn solve2-loop [input]
  (let [head (solve1 input)
        name-map (input->map input)]
    (loop [h head last-correct nil]
      (let [supporting (-> (name-map h) :supporting)
            supporting-weights ;; {weight -> [[name  weight] ... ]}
            (group-by second
                      (map #(vector % (tally-chain name-map %))
                           supporting))
            wrong-weight
            (filter #(= 1 (count (second %)))
                    supporting-weights)
            correct-weight
            (filter #(not= 1 (count (second %)))
                    supporting-weights)]
        (if (empty? wrong-weight)
          (let [tail-weight (tally-chain name-map h)
                head-weight (-> (name-map h) :weight)
                adjusted-head-weight (+ head-weight
                                        (- last-correct
                                           tail-weight))]
            {:unbalanced-node (name-map h)
             :incorect-weight tail-weight
             :correct-weight last-correct
             :adjusted-head-weight adjusted-head-weight})
          (recur (-> wrong-weight first second first first)
                 (-> correct-weight first first)))))))
