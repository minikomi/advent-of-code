(ns day9
  (:require
   [clojure.string :as str]
   [dom-top.core :refer [loopr]]
   [clojure.math.combinatorics :as combo]
   [instaparse.core :as insta]))

(def input1 "7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3")


(defn parse [s]
  (let [grammar "main = numberline+
numberline = number <','> number <'\n'?>
number = #'[0-9]+'"
        parser (insta/parser grammar)
        parse-tree (parser s)]
    (insta/transform
     {:main vector
      :numberline vector
      :number clojure.edn/read-string}
     parse-tree)))

(defn rectangle-size [[^long x1 ^long y1][^long x2 ^long y2]]
  (*
   (inc (apply - (sort > [x1 x2])))
   (inc (apply - (sort > [y1 y2])))))

(comment (parse input1))

(comment
  (rectangle-size [2 5] [9 7])

  )



(defn build-distances [points]
  (let [collect (volatile! [])]
    (doseq [idx (range (count points))
            :let [p1 (nth points idx)]
            p2 (subvec points (inc idx))
            :when (and (not= p1 p2)
                       (not= (first p1) (first p2))
                       (not= (second p1) (second p2)))
            :let [size (rectangle-size p1 p2)]]
      (vswap! collect conj [size p1 p2]))
    (sort @collect)))

(last (build-distances (parse input1)))

(comment
  (last (build-distances (parse (str/trim (slurp "./inputs/day9.txt")))))
  )

(defn adjust-point [[x y] dir]
  (case dir
    :r [(inc x) y]
    :d [x (inc y)]
  ))

(< 1 2)

(defn flood-fill [points]
  (let [pts-set (set points)
        max-x (apply max (map first points))
        max-y (apply max (map second points))
        start-p (first points)
        ]
    (loop [
           [x y] start-p
           start-points [[start-p :d] [start-p :r]]
           chain [start-p]
           collected #{}
           unvisited (rest points)]
       (println [x y] chain collected (second (peek start-points)))
     (cond
       
       (and (empty? start-points)
            (empty? unvisited)) collected
       
       (empty? start-points)
       (let [new-point (first unvisited)]
           (recur
            new-point
                  [[new-point :d] [new-point :r]]
                  [new-point]
                  collected
                  (rest unvisited)))

       (and (= :r (second (peek start-points)))
            (< max-x x))
       (let [new-start-points (pop start-points)
             [p _] (peek new-start-points)]
         (recur
          p
                new-start-points
                [p]
                collected
                unvisited))

       (and (= :d (second (peek start-points)))
            (< max-y y))
       (let [new-start-points (pop start-points)
             [p _] (peek new-start-points)]
         (recur

          p
                new-start-points
                [p]
                collected
                unvisited))

       :else (let [[_ dir] (peek start-points)
                   new-point (adjust-point [x y] dir)
                   is-point (and (pts-set new-point)
                                 (not (collected new-point)))
                   new-chain (conj chain new-point)]
               (

                recur
                new-point
                (if is-point
                  (conj start-points
                        [new-point :d]
                        [new-point :r]
                        )
                  start-points)
                [new-point]
                
                (if is-point
                  (into collected new-chain)
                  collected)
                
                unvisited
                )

               )))))

(flood-fill (parse input1))

(defn visualize [points]
  (let [p-set (set points)
        max-x (apply max (map first points))
        max-y (apply max (map second points))]
    (doseq [y (range (inc max-y))
            :let [_ (print "\n")]
            x (range (inc max-x))]
      (if (p-set [x y])
        (print "#")
        (print "."))
      (flush)

      )
    (flush)

    )

  )

(visualize (flood-fill (parse input1)))
