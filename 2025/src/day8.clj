(ns day8
  (:require
   [clojure.string :as str]
   [dom-top.core :refer [loopr]]
   [instaparse.core :as insta]))

(def input1 "162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689")

(defn parse [s]
  (let [grammar "main = numberline+
numberline = number (<','> number)* <'\n'?>
number = #'[0-9]+'"
        parser (insta/parser grammar)
        parse-tree (parser s)]
    (insta/transform
     {:main vector
      :numberline vector
      :number clojure.edn/read-string}
     parse-tree)))

(comment (parse input1))

(defn distance [[^long x1 ^long y1 ^long z1]
                [^long x2 ^long y2 ^long z2]]
  (Math/sqrt
   (+ (Math/pow (- x1 x2) 2)
      (Math/pow (- y1 y2) 2)
      (Math/pow (- z1 z2) 2))))

(defn build-distances [points]
  (let [collect (volatile! [])]
    (doseq [idx (range (count points))
            :let [p1 (nth points idx)]
            p2 (subvec points (inc idx))
            :when (not= p1 p2)
            :let [dist (distance p1 p2)]]
      (vswap! collect conj [dist p1 p2]))
    (sort @collect)))

(defn build-network [edges]
  (loopr [point-to-chain-map {}
          n 0
          last-connection nil]
         [[dist p1 p2] edges]
         (let [chain1 (point-to-chain-map p1)
               chain2 (point-to-chain-map p2)]
           (cond
             (and chain1 chain2 (= chain1 chain2))
             (recur point-to-chain-map n last-connection)

             (and chain1 chain2)
             (recur (update-vals point-to-chain-map #(if (= % chain2) chain1 %))
                    n
                    last-connection)

             chain1
             (recur (assoc point-to-chain-map p2 chain1)
                    n
                    [p1 p2])

             chain2
             (recur (assoc point-to-chain-map p1 chain2)
                    n
                    [p1 p2])

             :else
             (recur (assoc point-to-chain-map p1 n p2 n)
                    (inc n)
                    [p1 p2])))
         {:point-to-chain-map point-to-chain-map
          :n n
          :last-connection last-connection}))

(defn solve1 [points n-connections n-chains]
  (->>  (build-distances points)
        (take n-connections)
        build-network
        :point-to-chain-map
        (group-by second)
        (map (comp count second))
        (sort >)
        (take n-chains)
        (reduce *)))

(comment
  (solve1  (parse input1) 10 3)
  (solve1 (parse (str/trim (slurp "./inputs/day8.txt"))) 1000 3)
  ;;
  )

(defn solve2 [points]
  (->>  (build-distances points)
        build-network
        :last-connection
        (map first)
        (reduce *)))

(comment
  (solve2 (parse input1))
  (solve2 (parse (str/trim (slurp "./inputs/day8.txt"))))
  ;;
  )


