(ns advent.2017.day3)

(def squares
  (map #(* % %) (range)))

(def odd-squares
  (filter odd? squares))

(defn straight-edge-numbers [layer-number]
  (last
   (map-indexed
    (fn [n v] (mapv #(+ v (* 2 n %)) (range 4)))
    (reduce #(conj % (+ %2 (last %)))
            [1]
            (map #(inc (* 8 %)) (range layer-number))))))

(defn get-layer [n]
  (int (Math/floor (/ (Math/ceil (Math/sqrt n)) 2))))

(defn spiral-memory [n]
  (let [layer-number (get-layer n)
        se (straight-edge-numbers layer-number)
        closest-se (first (sort-by #(Math/abs (- % n)) se))]
    (+ (Math/abs (- closest-se n))
       layer-number)))

(def start-state
  {:pos [0 0]
   :vs {[0 0] 1}
   :idx 1
   :last-v 1
   :dir :right})

(defn do-turn [dir]
  (case dir
    :right :up
    :up    :left
    :left  :down
    :down  :right))

(defn do-move [[x y] dir]
  (case dir
    :right [(inc x)      y]
    :up    [     x  (dec y)]
    :left  [(dec x)      y]
    :down  [     x  (inc y)]
    :nw [(dec x) (dec y)]
    :ne [(inc x) (dec y)]
    :sw [(dec x) (inc y)]
    :se [(inc x) (inc y)]))

(defn get-neighbouring-vs [pos vs]
  (for [mv [:up :down :left :right :ne :nw :sw :se]
        :let [mvd (do-move pos mv)]]
    (get vs mvd 0)))

(defn is-corner? [idx]
  (contains?
   (set
    (map #(- (get-max-sq-for-layer (get-layer idx))
             (* 2 (get-layer idx) %)) (range 1 4)))
   idx))

(defn get-max-sq-for-layer [n]
  (int (Math/pow (dec (* (inc n) 2)) 2)))

(defn step-state [{:keys [pos vs dir idx]}]
  (let [new-idx (inc idx)
        new-pos (do-move pos dir)
        layer (get-layer new-idx)
        corners (get-corners layer)
        new-dir (cond
                  (not= (get-layer idx)
                        (get-layer new-idx)) :up
                  (is-corner? new-idx) (do-turn dir)
                  :else dir)
        neighbours (get-neighbouring-vs new-pos vs)
        new-v (apply + neighbours)
        new-vs (assoc vs new-pos new-v)]
    (when (vs new-pos) (throw (Exception. (str new-pos))))
    {:idx new-idx
     :pos new-pos
     :dir new-dir
     :last-v new-v
     :vs new-vs}))

(comment
  (loop [s start-state]
    (if (< 347991 (:last-v s)) (:last-v s)
        (recur (step-state s)))))
