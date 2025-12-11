(ns day
  (:require
   [clojure.string :as str]
   [dom-top.core :refer [loopr]]
   [instaparse.core :as insta]))

(def input1 "")

(defn parse [s]
  (let [grammar "
                   number = #'[0-9]+'"
        parser (insta/parser grammar)
        parse-tree (parser s)]
    (insta/transform
     {:number clojure.edn/read-string}
     parse-tree)))

(defn solve1 [input-str])

(comment
  (solve1 (slurp "./inputs/day1.txt")))
