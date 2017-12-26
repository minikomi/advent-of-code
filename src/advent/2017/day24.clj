(ns advent.2017.day24
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.pprint :refer [pprint]]
            [clojure.set :as set]))


(defn parse-line [l]
  (mapv read-string (s/split l #"\/")))

(def test-input "0/2
2/2
2/3
3/4
3/5
0/1
10/1
9/10")

(def input-raw (slurp (io/resource "day24.txt")))

(def test-ports (mapv parse-line (s/split-lines test-input)))

(defn get-tail [chain]
  (if (empty? chain) 0
    (second (peek chain))))

(defn get-joinable [ports seen chain]
  (let [tail (get-tail chain)]
    (not-empty
     (filter #((set %) tail)
             (remove seen ports)))))

(defn get-port-tree [ports]
  (tree-seq
   (fn [{:keys [chain seen]}]
     (not-empty (get-joinable ports seen chain)))
   (fn [{:keys [chain seen]}]
     (for [[jh jt :as n] (get-joinable ports seen chain)
           :let [new-node
                 (if (= jh (get-tail chain))
                   [jh jt]
                   [jt jh])]]
       {:seen (conj seen n)
        :chain (conj chain new-node)}))
   {:chain []
    :seen #{}}))

(defn tally-port-chain [{:keys [chain]}]
  (apply + (flatten chain)))

(defn solve1 [input]
  (->> (map parse-line (s/split-lines input))
       get-port-tree
       (map tally-port-chain)
       (apply max)))

(comment
  (apply max-key tally-port-chain (get-port-tree test-ports))
  (solve1 input-raw)

  )
