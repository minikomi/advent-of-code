(ns advent.2017.day6)

(def input
  (get-indexed-max [4 10 4 1 8 4 9 14 5 1 14 15 0 15 3 5]))

(def test-input
  [0 2 7 0])

(defn get-indexed-max [memory-state]
  (reduce
   (fn indexed-max-reduce [[_ current-max :as indexed-max] idx]
     (let [v (get memory-state idx)]
       (if (< current-max v)
         [idx v]
         indexed-max)))
   [nil -1]
   (range (count memory-state))))

(comment
  (get-indexed-max test-input) ;; [2 7]
  (get-indexed-max input)      ;; [11 15])

(defn step [memory-state]
  (let [[max-idx max-val] (get-indexed-max memory-state)]
    (reduce
     (fn memory-update [arr offset]
       (update arr (mod offset (count memory-state)) inc))
     (assoc memory-state max-idx 0)
     (range (inc max-idx) (+ 1 max-idx max-val)))))

(defn solve [memory-state]
  (loop [current-memory memory-state
         seen           {memory-state 0}]
    (let [new-state (step current-memory)]
      (if (seen new-state)
        {:cycle-count   (count seen)
         :loop-distance (- (count seen) (seen new-state))}
        (recur new-state
               (assoc seen new-state (count seen)))))))

(comment (solve test-input))

(comment (time (solve input)))
