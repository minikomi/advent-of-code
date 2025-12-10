(ns day10
  (:require
   [clojure.string :as str]
   [clojure.java.shell :as sh]
   [dom-top.core :refer [loopr]]
   [clojure.math.combinatorics :as combo]
   [instaparse.core :as insta]))

(def MINIZINC
  "/Applications/MiniZincIDE.app/Contents/Resources/minizinc")

(def input1 "
[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}
")

(defn parse [s]
  (let [grammar "main = line+
line = switches <' '> button-effects <' '> joltage-requirements <whitespace>*
whitespace = #'[\\s\n\t]'
joltage-requirements = <'{'> numbers <'}'>
button-effects =  button-effect (<' '> button-effect)*
button-effect =  <'('> numbers <')'>
switches = <'['> (switch-on | switch-off)+ <']'>
switch-on = '#'
switch-off = '.' 
<numbers> = number (<','> number)*
number = #'[0-9]+'"
        parser (insta/parser grammar)
        parse-tree (parser (str/trim s))]
    (insta/transform
     {:main vector
      :line (fn [& vs] (reduce
                        (fn [m [k & vs]] (assoc m k (vec vs)))
                        {}
                        vs))
      :button-effect vector
      :switch-on (constantly 1)
      :switch-off (constantly 0)
      :number clojure.edn/read-string}
     parse-tree)))

(comment
  (parse input1)
  ;;
  )

(defn gen-all-size-permutations [items]

  (mapcat
   (fn [size]
     (combo/selections items size))
   (range 1 (inc (count items)))))

(comment (gen-all-size-permutations [1 2 3]))

(defn flip-button-at [switches n]
  (update switches n (fn [b] ({1 0 0 1} b))))

(defn apply-button [switches button-effect]
  (reduce
   (fn [sw n] (flip-button-at sw n))
   switches
   button-effect))

(comment
  (flip-button-at [1 0 0 1] 0)
  (apply-button [0 0 0 0] [1 3])
  ;;
  )

(defn run-simulation [switches button-effects]
  (reduce apply-button switches button-effects))

(comment
  (run-simulation [1 0 0 0 0 0] [[0 3 4]])
    ;;
  )

(defn find-button-combination [{:keys [switches button-effects]}]
  (let [all-permutations (gen-all-size-permutations button-effects)]
    (transduce
     (filter
      (fn [button-effect-combo]
        (= (run-simulation (vec (repeat (count switches) 0)) button-effect-combo)
           switches)))
     (completing (fn [acc v] (reduced v)))
     nil
     all-permutations)))

(comment
  (reduce + (map count (map find-button-combination
                            (parse input1))))

  (reduce + (map count (map find-button-combination
                            (parse (str/trim (slurp "./inputs/day10.txt"))))))
    ;;
  )
(defn run-simulation-track-joltage [switches joltage button-effects]
  (reduce (fn [[switches joltage] button]
            [(apply-button switches button)
             (reduce #(update % %2 inc) joltage button)])
          [switches joltage]
          button-effects))

(comment
  (run-simulation-track-joltage [1 0 0 0 0 0] [0 0 0 0 0 0] [[0 3 4] [1 2]])
    ;;
  )

(defn find-button-combination-satisfy-joltage [{:keys [switches button-effects]}]
  (let [all-permutations (gen-all-size-permutations button-effects)]
    (transduce
     (filter
      (fn [button-effect-combo]
        (let [[final-switches final-joltage]
              (run-simulation-track-joltage
               (vec (repeat (count switches) 0))
               (vec (repeat (count switches) 0))
               button-effect-combo)]
          (and (= final-joltage switches)))))
     (completing (fn [acc v] (reduced v)))
     nil
     all-permutations)))

(comment
  (map find-button-combination-satisfy-joltage
       (parse input1))
  (solve1 (slurp "./inputs/day1.txt")))

(defn create-minizinc-data
  "Given a parsed map with keys:
     :switches               => vector of 0/1 (length W)
     :button-effects         => seq of vectors/lists of 0-based indices (length M)
     :joltage-requirements   => seq of ints (length W)
   Return a string containing a .dzn data file for MiniZinc.
   Buttons are columns; counters (switches) are rows. Row-major order is used."
  [{:keys [switches button-effects joltage-requirements]}]
  (let [W (count switches)
        M (count button-effects)
        ;; Make a vector of sets so membership tests are cheap
        btn-sets (mapv #(set %) button-effects)
        ;; target string (with spaces for readability)
        target-str (str "[" (str/join ", " switches) "]")
        ;; build rows: for each counter i (0..W-1), for each button j (0..M-1)
        rows (for [i (range W)]
               (->> (range M)
                    (map (fn [j] (if (contains? (btn-sets j) i) "1" "0")))
                    (str/join ", ")))
        ;; flatten rows into one comma-separated block with newlines per row
        effects-block (str/join ",\n    " rows)
        joltage-str (str "[" (str/join ", " joltage-requirements) "]")]
    (format "W = %d;\nM = %d;\n\ntarget = %s;\n\neffects = array2d(1..W, 1..M,\n  [\n    %s\n  ]\n);\n\njoltage = %s;\n"
            W M target-str effects-block joltage-str)))

(println (create-minizinc-data (first (parse

                                       (str/trim (slurp "./inputs/day10.txt"))))))

(defn run-minizinc-data
  ([machine-spec] (run-minizinc-data machine-spec "instance.dzn"))
  ([machine-spec dzn-path]
   (let [dzn-text (create-minizinc-data machine-spec)]
     ;; write the dzn file
     (spit dzn-path dzn-text)
     ;; call MiniZinc
     (sh/sh MINIZINC "./data/min-presses.mzn" dzn-path))))

(comment

  (reduce + (map-indexed (fn [idx d] (Integer/parseInt (second (str/split
                                                                (first (take-last 3
                                                                                  (str/split-lines
                                                                                   (:out (run-minizinc-data d (str "/tmp/" idx ".data"))))))
                                                                #" "))))
                         (parse (str/trim (slurp "./inputs/day10.txt"))))))


