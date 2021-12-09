(ns advent.2021.day6
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def start-test-school [3 4 3 1 2])

(def parsed-input (as-> "2021/input6.txt" in
                    (io/resource in)
                    (slurp in)
                    (str/split in #",")
                    (mapv #(Integer/parseInt (str/trim %)) in)))

(defn initialize-school [fs]
  (reduce #(update % (- 8 %2) inc)
          (vec (repeat 9 0))
          fs))

(defn step-school [fs]
  (let [fs' (pop fs)
        mama-f (peek fs)]
    (-> (into [mama-f] fs')
        (update 2 + mama-f))))

(defn advent [start-school days]
  (let [s (volatile! (initialize-school start-school))
        f #(vswap! s step-school)]
    (doall (repeatedly days f))
    (apply + @s)))

(comment
  (println (advent parsed-input 80))
  (println (advent parsed-input 256)))
