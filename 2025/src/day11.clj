(ns day11
  (:require
   [clojure.string :as str]
   [dom-top.core :refer [loopr]]
   [clojure.math.combinatorics :as combo]
   [instaparse.core :as insta]
   [clojure.set :as set]))

(def input1 "aaa: you hhh
you: bbb ccc
bbb: ddd eee
ccc: ddd eee fff
ddd: ggg
eee: out
fff: out
ggg: out
hhh: ccc fff iii
iii: out")

(defn parse [s]
  (let [grammar " main: line+
line: id <':'> (<' '> id)+ <'\n'?>
id : #'[a-z]+'"
parser (insta/parser grammar)
        parse-tree (parser s)]
    (insta/transform
     {:main (fn [& ls] (into {} ls))
      :id keyword
      :line (fn [n & ms] [n (vec ms)])}
     parse-tree)))

(comment (parse input1)
;;
         )

(defn solve1 [connection-map start-node]
 (let [collect (volatile! [])]
   (doall
    (tree-seq
     (fn branch? [c]
       (if (= (peek c) :out)
         (do (vswap! collect conj c)
             false)
         true))
     (fn children [n]
       (mapv #(conj n %)
             (connection-map (peek n))))
     [start-node]))
   @collect))

(def input2 "svr: aaa bbb
aaa: fft
fft: ccc
bbb: tty
tty: ccc
ccc: ddd eee
ddd: hub
hub: fff
eee: dac
dac: fff
fff: ggg hhh
ggg: out
hhh: out")

(defn solve2-too-slow [connection-map start-node]
  (let [paths-count (volatile! 0)]
    (doall
     (tree-seq
      (fn branch? [[n seen-fft-or-dac]]
         (cond
           (= :out n)
           (do (when (= 2 seen-fft-or-dac)
                 (vswap! paths-count inc))
               false) 
           :else true))
      (memoize
       (fn children [[n seen-fft-or-dac]]
         (map (fn [n']
                [n'  
                 (if (or (= n' :fft)
                         (= n' :dac))
                   (inc seen-fft-or-dac)
                   seen-fft-or-dac)])
              (connection-map n))))
      [start-node 0]))
    @paths-count))

(defn count-paths [!cache connection-map node seen-fft? seen-dac?]
  (let [fft? (or seen-fft? (= node :fft))
        dac? (or seen-dac? (= node :dac))]
    (or (@!cache [node fft? dac?])
        (let [result
              (if (= node :out)
                (if (and fft? dac?) 1 0)
                (->> (connection-map node)
                     (map #(count-paths !cache connection-map % fft? dac?))
                     (reduce + 0)))]
          (vswap! !cache assoc [node fft? dac?] result)
          result))))

(defn solve2 [connection-map start-node]
  (let [!cache (volatile! {})] ;; [node fft? dac?] -> succeses cache
    (count-paths !cache connection-map start-node false false)))

(comment
  (count (solve1 (parse (slurp "./inputs/day11.txt")) :you))
  (solve2 (parse input2) :svr)
  (solve2-too-slow (parse input2 ) :svr)
  (solve2 (parse (slurp "./inputs/day11.txt")) :svr) 

  )
