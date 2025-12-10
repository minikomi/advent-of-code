(ns day9
  (:require
   [clojure.string :as str]
   [dom-top.core :refer [loopr]]
   [clojure.set :as set]
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

(defn rectangle-size [[^long x1 ^long y1] [^long x2 ^long y2]]
  (*
   (inc (apply - (sort > [x1 x2])))
   (inc (apply - (sort > [y1 y2])))))

(comment (parse input1))

(comment
  (rectangle-size [2 5] [9 7]))

(defn build-sizes [points]
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

(last (build-sizes (parse input1)))

(comment
  (last (build-sizes (parse (str/trim (slurp "./inputs/day9.txt"))))))

(defn join-segment
  "Return both:
     - :points  => all integer boundary points on the segment
     - :segment => {:type :vertical|:horizontal
                    :x or :y
                    :min
                    :max}
   Guaranteed orthogonal."
  [[x1 y1] [x2 y2]]

  (cond
    ;; vertical edge
    (= x1 x2)
    (let [x x1
          ymin (min y1 y2)
          ymax (max y1 y2)
          pts  (set (for [y (range ymin (inc ymax))] [x y]))]
      {:points pts
       :segment {:type :vertical :x x :y-min ymin :y-max ymax}})

    ;; horizontal edge
    (= y1 y2)
    (let [y y1
          xmin (min x1 x2)
          xmax (max x1 x2)
          pts  (set (for [x (range xmin (inc xmax))] [x y]))]
      {:points pts
       :segment {:type :horizontal :y y :x-min xmin :x-max xmax}})

    :else
    (throw "hmm??")))

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
      (flush))
    (flush)))

(defn close-loop [pts]
  (let [pairs (partition 2 1 (conj pts (first pts)))]
    (reduce
     (fn [{:keys [all vertical-segs horizontal-segs
                  min-x max-x min-y max-y] :as acc}
          [p1 p2]]
       (let [{:keys [points segment]} (join-segment p1 p2)
             ;; update global min/max
             xs (map first points)
             ys (map second points)
             acc (-> acc
                     (update :all into points)
                     (update :min-x #(if % (min % (apply min xs)) (apply min xs)))
                     (update :max-x #(if % (max % (apply max xs)) (apply max xs)))
                     (update :min-y #(if % (min % (apply min ys)) (apply min ys)))
                     (update :max-y #(if % (max % (apply max ys)) (apply max ys))))]
         (case (:type segment)
           :vertical   (update acc :vertical-segs conj segment)
           :horizontal (update acc :horizontal-segs conj segment))))
     {:all #{}
      :vertical-segs []
      :horizontal-segs []
      :min-x nil
      :max-x nil
      :min-y nil
      :max-y nil}
     pairs)))

(comment
  (def points (parse input1))
  (def closed (close-loop points))

  (visualize (:all closed)))

(defn inside?
  [{:keys [all vertical-segs min-x max-x min-y max-y]} [px py]]
  (cond
    (contains? all [px py]) true
    (or (< px min-x) (> px max-x)
        (< py min-y) (> py max-y)) false
    :else
    (let [crossings
          (count
           (filter (fn [{:keys [x y-min y-max]}]
                     (and (> x px)
                          (<= y-min py)
                          (<  py y-max)))
                   vertical-segs))]
      (odd? crossings))))

(defn four-corners-inside?
  [p1 p2 desc]
  (let [[x1 y1] p1
        [x2 y2] p2
        corners [[x1 y1] [x2 y2] [x1 y2] [x2 y1]]]
    (every? #(inside? desc %) corners)))

(defn rectangle-edges-inside?
  [p1 p2 ranges-map boundary-set]
  (let [[x1 y1] p1
        [x2 y2] p2
        xmin (min x1 x2) xmax (max x1 x2)
        ymin (min y1 y2) ymax (max y1 y2)]

    (and
      ;; Top edge: y = ymax, x in [xmin, xmax]
     (every? (fn [x]
               (or (x-in-ranges? (get ranges-map ymax) x)
                   (contains? boundary-set [x ymax])))
             (range xmin (inc xmax)))

      ;; Bottom edge: y = ymin, x in [xmin, xmax]
     (every? (fn [x]
               (or (x-in-ranges? (get ranges-map ymin) x)
                   (contains? boundary-set [x ymin])))
             (range xmin (inc xmax)))

      ;; Left edge: x = xmin, y in [ymin, ymax]
     (every? (fn [y]
               (or (x-in-ranges? (get ranges-map y) xmin)
                   (contains? boundary-set [xmin y])))
             (range ymin (inc ymax)))

      ;; Right edge: x = xmax, y in [ymin, ymax]
     (every? (fn [y]
               (or (x-in-ranges? (get ranges-map y) xmax)
                   (contains? boundary-set [xmax y])))
             (range ymin (inc ymax))))))

(defn solve-two-pass
  [points]
  (let [desc (close-loop points)
        ranges-map (build-scanline-ranges desc)
        boundary (:all desc)

        ;; PASS 1: Quick corner check - filters out most bad rectangles
        candidates-after-corners
        (for [i (range (count points))
              j (range (inc i) (count points))
              :let [p1 (points i) p2 (points j)]
              :when (and (not= (first p1) (first p2))
                         (not= (second p1) (second p2)))
              ;; Fast rejection: 4 corners must be inside
              :when (four-corners-inside? p1 p2 desc)]
          [p1 p2])

        _ (println "Candidates after corner check:" (count candidates-after-corners))

        candidates-in-size-order (sort-by :area >
                                          (map
                                           (fn [[p1 p2]]
                                             {:p1 p1
                                              :p2 p2
                                              :area (rectangle-size p1 p2)})
                                           candidates-after-corners))]

;; PASS 2: find the first which passes full edge check
    (some (fn [{:keys [p1 p2 area] :as rect}]
            (when (and (four-corners-inside? p1 p2 desc)
                       (rectangle-edges-inside? p1 p2 ranges-map boundary))
              (do
                (println "Found maximum rectangle with area:" area)
                rect)))
          candidates-in-size-order)))

(comment
  (solve-two-pass (parse
                   (str/trim (slurp "./inputs/day9.txt")))))

