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
  (reduce
   (fn [rulebook rule-line]
     (let [{:keys [input output]} (parse-rule rule-line)]
       (reduce conj rulebook (map vector input (repeat output)))))
   {}
   (s/split-lines input-raw)))

(defn split [mtx n]
  (->> mtx
       (map #(partition n %))
       (apply mapcat vector)
       (map vec)
       (partition n)
       (map vec)))

(def r (parse-rule test-rule-2))

(defn step [rules mtx]
  (when-let [split-val (cond
                         (zero? (mod (count mtx) 2)) 2
                         (zero? (mod (count mtx) 3)) 3
                         :else false)]
    (let [mtx-parts (split mtx split-val)
          expanded-parts
          (for [m mtx-parts
                :let [rot (take 4 (iterate rotate m))
                      flp (map mirror rot)]]
            (or (first (keep rules flp))
                (first (keep rules rot))))
          expand-count (/ (count mtx) split-val)]
      (vec
       (mapcat
        #(apply map concat (repeat []) %)
        (partition expand-count expanded-parts))))))

(def input-raw (slurp (io/resource "day21.txt")))

(let [r (parse-rules input-raw)]
  (count (filter #(= \# %)
                 (flatten (nth (iterate (partial step r) seed-matrix)
                               5)))))

(let [r (parse-rules input-raw)]
  (count (filter #(= \# %)
                 (flatten (nth (iterate (partial step r) seed-matrix)
                               18)))))
