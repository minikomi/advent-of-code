(ns advent.2022.day7
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [clojure.java.io :as io]))

(def test-input (str/split-lines "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k"))

(defn parse-command [state cmd]
  (cond
    ;; ignore
    (str/starts-with? cmd "$ ls") state
    (= cmd "$ cd ..") (update state :path pop)
    (str/starts-with? cmd "$ cd") (update state :path conj (subs cmd 5))
    (str/starts-with? cmd "dir")
    (let [v (subs cmd 4)]
      (update state :structure update-in (:path state) assoc v {}))
    :else (let [v (Integer/parseInt (first (str/split cmd #" ")))
                dirs' (reduce (fn [d p]
                                (update d (apply str (interpose "/" p)) (fnil + 0) v))
                              (:dirs state)
                              (reductions conj ["."] (rest (:path state))))]
            (->  (assoc state :dirs dirs')
                 (update :structure update-in (conj (:path state) :files) (fnil conj []) v)))))

(defn solve [input]
  (let [dirs (:dirs (reduce parse-command {:path [] :dirs {} :structure {}} input))]
    (->>
     (map second dirs)
     (filter #(>= 100000 %))
     (reduce +))))

(def input (->> (io/resource "2022/day7-input1.txt")
                slurp
                str/split-lines))

(comment
  (solve test-input)
  (reduce parse-command {:path [] :dirs {}} test-input)
  (solve input)
  ;;
  )

(defn solve2 [input]
  (let  [structure (reduce parse-command {:path [] :dirs {}} input)
         at-least (- 30000000 (- 70000000 (get-in structure [:dirs "."])))
         filtered (filter #(< at-least (second %)) (:dirs structure))]
    (first (sort (map second filtered)))))

(comment
  (solve2 input)
  ;;
  )
