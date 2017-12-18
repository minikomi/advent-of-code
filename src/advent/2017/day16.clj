(ns advent.2017.day16
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [advent.util :as util]))

(def input (slurp (io/resource "day16.txt")))

;; - Spin,     written sX, makes X programs move from the end to the front,
;;             but maintain their order otherwise. (For example, s3 on abcde produces cdeab).
;; - Exchange  written xA/B, makes the programs at positions A and B swap places.
;; - Partner   written pA/B, makes the programs named A and B swap places.

(defn spin [programs instruction]
  (let [n (read-string (subs instruction 1))]
    (into
     (subvec programs (- (count programs) n))
     (subvec programs 0 (- (count programs) n)))))

(comment
  (spin (vec "abcde") "s3")
  (spin (vec "abcde") "s1")
  )

(defn swap [programs instruction]
  (let [[_ a* b*] (re-matches  #"x(\d+)\/(\d+)" instruction)
        a (read-string a*)
        b (read-string b*)]
    (assoc programs
           a (get programs b)
           b (get programs a))))

(comment
  (swap (vec "abcde") "x3/4")
  )

(defn partner [programs instruction]
  (let [a (-> instruction (nth 1))
        b (-> instruction (nth 3))]
    (mapv #(cond
             (= % a) b
             (= % b) a
             :else %)
          programs)))

(defn nibble [programs instruction]
  (case (first instruction)
    \s (spin programs instruction)
    \x (swap programs instruction)
    \p (partner programs instruction)
    (do (println instruction)
        (throw (Exception. "unknown command")))))

(defn solve1 [programs instructions]
  (reduce nibble
          programs
          instructions))

(defn solve2 [programs instructions]
  (let [[loop-size possible-states]
        (loop [current programs seen {programs 0} n 1]
          (let [new-state (reduce nibble current instructions)]
            (if (seen new-state) [n seen]
                (recur new-state
                       (assoc seen new-state n)
                       (inc n)))))
        which-state (int (mod 1e9 loop-size))]
    (->> possible-states
         (filter (fn [[s n]] (= which-state n)))
         ffirst
         (apply str))))

(comment
  (solve1 "abcde" "s1,x3/4,pe/b")
  (solve1 (vec "abcdefghijklmnop") (vec (s/split input #",")))
  (solve2 (vec "abcdefghijklmnop") (vec (s/split input #",")))
  )
