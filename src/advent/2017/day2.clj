(ns advent.2017.day2
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn process-row [row]
  (- (apply max row)
     (apply min row)))

(defn checksum [rows]
  (apply + (map process-row rows)))

(def input
  (->> (io/resource "day2.txt")
       slurp
       s/split-lines
       (mapv #(mapv read-string (s/split % #"\t")))))

(comment
  (assert (= 8
             (process-row [5 1 9 5])))
  (assert (= 4
             (process-row [7 5 3])))
  (assert (= 6
             (process-row [2 4 6 8])))
  (assert (= 18
             (checksum
              [[5 1 9 5]
               [7 5 3]
               [2 4 6 8]]))))

(defn throw-row-error [row]
  (throw (Exception. (format "Couldn't find evenly divisible: %s\n" row))))

(defn find-evenly-divisible [row]
  (loop [r (-> row sort distinct)]
    (if (empty? r) (throw-row-error row)
        (or
         (first
          (filter #(and (zero? (mod % (first r)))
                        [% (first r)])))
         (recur (rest r))))))

(defn process-row-2 [row]
  (apply / (find-evenly-divisible row)))

(defn spreadsheet-process [rows]
  (apply + (map process-row-2 rows)))

(comment
  (assert
   (= [8 2]
      (find-evenly-divisible [5 9 2 8])))
  (assert
   (= [18 9]
      (find-evenly-divisible [9 9 18])))
  (assert
   (= 4
      (process-row-2 [5 9 2 8])))
  (assert
   (= 9
      (spreadsheet-process [[5 9 2 8]
                            [9 4 7 3]
                            [3 8 6 5]]))))

(defn solve-day-2 []
  (println "2-1:"
           (checksum input))
  (println "2-2:"
           (spreadsheet-process input)))
