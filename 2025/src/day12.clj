(ns day12
  (:require
   [clojure.string :as str]
   [dom-top.core :refer [loopr]]
   [clojure.math.combinatorics :as combo]
   [instaparse.core :as insta]
   [clojure.set :as set]))

(def input1 "0:
###
##.
##.

1:
###
##.
.##

2:
.##
###
##.

3:
##.
###
##.

4:
###
#..
###

5:
###
.#.
###

4x4: 0 0 0 0 2 0
12x5: 1 0 1 0 2 2
12x5: 1 0 1 0 3 2")

(defn parse [s]
  (let [grammar "<main>: presents region-lines
region-lines : region-line+
presents = present+
present = <present-id> <'\n'> present-lines <'\n'*>
<present-lines>: present-line+
<present-id>: number+ <':'>
present-line = (present-there | present-blank)+ <'\n'>
<present-there> = '#'
<present-blank> = '.'
region-line = region-size region-present-requirements <'\n'*>
region-size: number <'x'> number <':'>
region-present-requirements: (<' '> number)+  
<numbers> = number (<' '> number)*
number = #'[0-9]+'
"
        
parser (insta/parser grammar)
        parse-tree (parser s)]
    (insta/transform
     {
      :presents vector
      :present vector
      :present-line (fn [& vs] (mapv {"#" 1 "." 0} vs))
      :region-lines vector
      :region-size vector
      :region-present-requirements vector
      :region-line (fn [size reqs]1
                     {:size size :reqs reqs}

                     )
      :number clojure.edn/read-string
      }
     parse-tree)))



(comment (clojure.pprint/pprint (parse input1))
;;
         )

(defn simple-check [shapes regions]
  (let [shape-sizes (mapv #(reduce + (flatten % )) shapes)]
    (for [{:keys [size reqs]} regions
           :let [area (apply * size)
                 reqs-size (apply + (map-indexed (fn [idx n] (* n (get shape-sizes idx))) reqs))]
           ]
       [area reqs-size (> area reqs-size)]
       )))

(comment
  (->> (parse (str/trim (slurp "./inputs/day12.txt")))
       (apply simple-check)
       second
       (filter #(nth % 2))
       count)
;;
  )



