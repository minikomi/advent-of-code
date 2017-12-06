(ns advent.2017.day6)

(def input
  (get-indexed-max [4 10 4 1 8 4 9 14 5 1 14 15 0 15 3 5]))

(def test-input
  [0 2 7 0])

(defn get-indexed-max [memory-state]
  (loop [vs memory-state
         idx 0
         current-max [nil -1]
         ]
    (if (empty? vs) current-max
        (recur (rest vs)
               (inc idx)
               (if (< (second current-max) (first vs))
                 [idx (first vs)]
                 current-max)))))

(comment (get-indexed-max input))

(defn step [memory-state]
  (let [[max-idx max-val] (get-indexed-max memory-state)]
    (reduce (fn [arr offset]
              (update arr (mod (+ max-idx offset) (count memory-state))
                      inc))
            (assoc memory-state max-idx 0)
            (range 1 (inc max-val)))))

(defn solve [memory-state]
  (loop [current-memory memory-state seen #{memory-state} cycles 0]
    (let [new-state (step current-memory)]
      (if (seen new-state) cycles
          (recur new-state
                 (conj seen new-state)
                 (inc cycles))))))


(defn solve2 [memory-state]
  (loop [current-memory memory-state seen {memory-state 0} cycles 0]
    (let [new-state (step current-memory)
          new-cycle-count (inc cycles)]
      (if (seen new-state) (- new-cycle-count (seen new-state))
          (recur new-state
                 (assoc seen new-state new-cycle-count)
                 new-cycle-count)))))

(comment (solve2 test-input))

(comment (println (solve2 input)))
