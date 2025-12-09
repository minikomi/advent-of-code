(ns advent.2021.day8
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set]))

(defn parse-input [input-s]
  (mapv (fn [s] (str/split (str/trim s) #" "))
        (str/split input-s #"\|")))

(def test-input "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce")

(def advent-1 (def parsed-input (mapv parse-input (str/split-lines))))

(defn advent-1 [input-s]
  (let [parsed-input (mapv parse-input (str/split-lines input-s))]
    (->>
     (mapv second)
     (reduce into [])
     (filter #(#{2 4 3 7} (count %)))
     count)))

(comment
  (advent-1 test-input)
  (advent-1 (slurp (io/resource "2021/input8.txt"))))

(def seg-map
  (into {}

        (map (fn [[k v]]
               [k (set v)])
             {0 "abcefg"
              1 "cf"
              2 "acdeg"
              3 "acdfg"
              4 "bcdf"
              5 "abdfg"
              6 "abdefg"
              7 "acf"
              8 "abcdefg"
              9 "abcdfg"})))

(set/difference (seg-map 7)
                (seg-map 1))

(defn seg-not-in [a b]
  (let [[a-seg b-seg] [(seg-map a) (seg-map b)]
        [[m more] [l less]] (if (> (count a-seg) (count b-seg))
                              [[a a-seg] [b b-seg]] [[b b-seg] [a a-seg]])]
    [[m more] [l less] (set/difference more less)]))
