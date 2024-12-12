(import ./util :reload-all)
(use judge)

(def input1 `2333133121414131402`)

(def day9-input (util/read-file "resources/day9.txt"))

(defn parse [input-str]
  (seq [i :range [0 (length input-str)]
        :let [n (scan-number (string/slice input-str i (+ i 1)))
              v (if (even? i) (/ i 2) nil)]
        _ :range [0 n]]
    v))

(defn print-map [m]
  (string/join (map |(if $ (string $) ".") m) ""))


(test (string/join (map |(if $ (string $) ".") (parse input1)) "")
      # 00...111...2...333.44.5555.6666.777.888899
      "00...111...2...333.44.5555.6666.777.888899")

(defn print-state [[l r m]]
  (print (print-map m))
  (print l)
  (print r)
  )

(defn solve [input-str]
  (var m (parse input-str))
  (var l 0)
  (var r (dec (length m)))
  (while (not= l r)
    (cond
      (= l r) :noop
      (nil? (m r)) (-- r)
      (nil? (m l)) (do
                     (-> m (put l (m r)) (put r nil))
                     (++ l)
                     (-- r))
      :else (++ l)))
  (var acc 0)
  (util/loopv [i :range [0 (length m)]
         :let [v (m i)]
         :until (nil? v)]
    (set acc (+ acc (* i v))))
  acc)

(solve day9-input)

(test (partition-by identity (parse input1))
  @[@[0 0]
    @[nil nil nil]
    @[1 1 1]
    @[nil nil nil]
    @[2]
    @[nil nil nil]
    @[3 3 3]
    @[nil]
    @[4 4]
    @[nil]
    @[5 5 5 5]
    @[nil]
    @[6 6 6 6]
    @[nil]
    @[7 7 7]
    @[nil]
    @[8 8 8 8]
    @[9 9]])

(defn solve2 [input-str]
  (var m (partition-by identity (parse input-str)))
  (var r (dec (length m)))
  (while (< 0 r)
    (cond
      (= 0 r) :noop
      (nil? (first (m r))) (-- r)
      :else
      (do
        (var l 0)
        (while (< l r)
          (if (not (nil? (get-in m [l 0]))) (++ l)
          (let [target (m l) src (m r)]
            (if (< (length target) (length src))
              (++ l)
              (do
                (put m r (array/new-filled (length src) nil))
                (set m
                     @[;(array/slice m 0 l)
                       ;(if (= (length target) (length src))
                          [src]
                          [src
                           (do
                             (++ r)
                             (let [rem (array/slice target (length src))]
                               rem))])
                       ;(drop (inc l) m)])
                (break)
                )))))
        (-- r)
        )))
  (var acc 0)
  (def f (flatten m))
  (util/loopv [i :range [0 (length f)]
               :let [v (f i)]
               :when (not (nil? v))]
              (set acc (+ acc (* i v))))
  acc)

(test (solve2 input1) 2858)
(test (solve2 day9-input) 6361209907931)

(test (solve2 "2833133121414131402") 23423)
