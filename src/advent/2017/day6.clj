(ns advent.2017.day6)

(def input
  (get-indexed-max [4 10 4 1 8 4 9 14 5 1 14 15 0 15 3 5]))

(def test-input
  [0 2 7 0])

(defn get-indexed-max [memory-state]
  (loop [idx 0
         current-max [nil -1]] ;; no values below 0
    (if-let [v (get memory-state idx)]
      (recur (inc idx)
             (if (< (second current-max) v)
               [idx v]
               current-max))
      current-max)))

(comment
  (get-indexed-max test-input) ;; [2 7]
  (get-indexed-max input)      ;; [11 15]
  )

(defn step [memory-state]
  (let [[max-idx max-val] (get-indexed-max memory-state)]
    (reduce (fn [arr offset]
              (update arr (mod (+ max-idx offset) (count memory-state))
                      inc))
            (assoc memory-state max-idx 0)
            (range 1 (inc max-val)))))

(defn solve [memory-state]
  (loop [current-memory memory-state seen {memory-state 0} cycles 0]
    (let [new-state (step current-memory)
          new-cycle-count (inc cycles)]
      (if (seen new-state)
        {:cycle-count new-cycle-count
         :loop-distance (- new-cycle-count (seen new-state))}
        (recur new-state
               (assoc seen new-state new-cycle-count)
               new-cycle-count)))))

(comment (solve test-input))

(comment (println (solve input)))
