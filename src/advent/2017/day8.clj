(ns advent.2017.day8
  (require [clojure.string :as s]
           [advent.util :refer [let-stop]]
           [clojure.java.io :as io]
           [clojure.walk :as w]))

(def test-input
  "b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10")

(defn parse-row [row]
  (let [[instruction-full condition] (s/split row #" if ")
        [reg-loc instruction value] (s/split instruction-full #"\ ")
        [cond-reg cond-test cond-val] (s/split condition #"\ ")]
    {:reg-loc reg-loc
     :instruction instruction
     :mod-value (read-string value)
     :cond-reg cond-reg
     :cond-test cond-test
     :cond-val (read-string cond-val)}))

(defn parse-input [input]
  (map
   parse-row
   (s/split-lines input)))

(defn test-instruction [{:keys [cond-test cond-val]} reg-val]
  (case cond-test
    "!="
    (not= reg-val cond-val)
    "=="
    (= reg-val cond-val)
    ((resolve (symbol cond-test)) reg-val cond-val)))

(defn step [registers {:keys [cond-reg reg-loc instruction mod-value]
                       :as row}]
  (if (test-instruction row (get registers cond-reg 0))
    (update registers
            reg-loc
            (fnil ({"inc" + "dec" -} instruction) 0)
            mod-value)
    registers))

(def input (slurp (io/resource "day8.txt")))

(defn solve1 [input]
  (->> (parse-input input)
       (reduce step {})
       (apply max-key second)))

(defn solve2 [input]
  (->>
   (parse-input input)
   (reductions step {})
   (map vals)
   (apply concat)
   (apply max)))
