(ns advent.2017.day21
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def test-rule-1 "../.# => ##./#../...")
(def test-rule-2 ".#./..#/### => #..#/..../..../#..#")

(defn printm [mtx]
  (doseq [line mtx]
    (doseq [c line]
      (print c))
    (println)))

(defn parse-matrix [m]
  (mapv vec (s/split-lines m)))

(def seed-matrix (parse-matrix ".#.\n..#\n###"))

(def test-matrix-2
  (parse-matrix "#..#\n....\n....\n#..#"))

(def test-matrix-3
  (parse-matrix ".#..#.\n..#..#\n######\n.#..#.\n..#..#\n######"))

(defn rotate [mtx]
  (apply mapv (comp vec reverse vector) mtx))

(defn mirror [mtx]
  (mapv (comp vec reverse) mtx))

(defn parse-rule [rule-line]
  (let [[input output] (s/split rule-line #" => ")
        input-parts (mapv vec (s/split input #"\/"))
        output-mtx (mapv vec (s/split output #"\/"))
        rot (take 4 (iterate rotate input-parts))
        flp (mapv mirror rot)]
    {:input (set (into flp rot))
     :output output-mtx}))

(defn parse-rules [input-raw]
  (map parse-rule (s/split-lines input-raw)))

(defn split [mtx n]
  (->> mtx
       (map #(map vec (partition n %)))
       (apply map vector)
       (map #(map vec (partition n %)))
       (apply mapcat vector)
       (vec)))

(defn join [expanded-parts expand-count]
  (vec
   (mapcat
    #(apply map concat %)
    (partition expand-count expanded-parts))))

(def lookup-rule
  (memoize (fn lookup-rule [rules m]
             (first (filter #((:input %) m) rules)))))

(defn step [rules mtx]
  (when-let [split-val (cond
                         (zero? (mod (count (first mtx)) 2)) 2
                         (zero? (mod (count (first mtx)) 3)) 3
                         :else false)]
    (let [mtx-parts (split mtx split-val)
          expanded-parts
          (for [m mtx-parts
                :let [rule (lookup-rule rules m)]]
            (:output rule))
          expand-count (/ (count mtx) split-val)]
      (join expanded-parts expand-count))))

(def input-raw (slurp (io/resource "day21.txt")))

(defn solve [input n]
  (let [r (parse-rules input)
        steps (iterate (partial step r) seed-matrix)
        final-step (nth steps n)]
    (count (filter #{\#} (flatten final-step)))))

(comment
  (solve input-raw 5)
  (solve input-raw 18)
  )
