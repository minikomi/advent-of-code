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
  (->> (name-map root)
       (tree-seq
        #(:supporting %)
        #(->> % :supporting (map name-map)))
       (map :weight)
       (apply +)))

(defn solve2 [input]
  (let [head (solve1 input)
        name-map (input->map input)]
    (loop [h head last-correct nil]
      (let [supporting (-> (name-map h) :supporting)
            supporting-weights ;; {weight -> [[name  weight] ... ]}
            (group-by second
                      (map #(vector % (walk-tally name-map %))
                           supporting))
            wrong-weight
            (filter #(= 1 (count (second %)))
                    supporting-weights)
            correct-weight
            (filter #(not= 1 (count (second %)))
                    supporting-weights)]
        (if (empty? wrong-weight)
          (let [tail-weight (walk-tally name-map h)
                head-weight (-> (name-map h) :weight)
                adjusted-head-weight (+ head-weight
                                        (- last-correct
                                           tail-weight))]
            {:name h
             :head-weight head-weight
             :tail-weight tail-weight
             :correct-weight last-correct
             :adjusted-head-weight adjusted-head-weight})
          (recur (-> wrong-weight first second first first)
                 (-> correct-weight first first)))))))

(comment (solve2 input))
