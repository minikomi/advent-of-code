(ns advent.2017.day23
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def input-raw
  "set b 93
set c b
jnz a 2
jnz 1 5
mul b 100
sub b -100000
set c b
sub c -17000
set f 1
set d 2
set e 2
set g d
mul g e
sub g b
jnz g 2
set f 0
sub e -1
set g e
sub g b
jnz g -8
sub d -1
set g d
sub g b
jnz g -13
jnz f 2
sub h -1
set g b
sub g c
jnz g 2
jnz 1 3
sub b -17
jnz 1 -23")

(defn parse-line [ln]
  (let [wrds (s/split ln #" ")]
    {:cmd (first wrds)
     :reg-a (read-string (second wrds))
     :reg-b (when-let [w (get wrds 2)]
              (read-string w))}))

(def input (map parse-line (s/split-lines input-raw)))

(defn inc-ptr [state]
  (update state :pointer inc))

(defn reg-get [registers r]
  (if (number? r) r
      (get registers r 0)))

;; commands

(defn do-set [{:keys [registers] :as state}
              {:keys [reg-a reg-b]}]
  (inc-ptr
   (update state :registers
           assoc
           reg-a
           (reg-get registers reg-b))))

(defn do-sub [{:keys [registers] :as state}
              {:keys [reg-a reg-b]}]
  (inc-ptr
   (update-in state [:registers reg-a]
              (fnil - 0)
              (reg-get registers reg-b))))

(defn do-mul [{:keys [registers] :as state}
              {:keys [reg-a reg-b]}]
  (inc-ptr
   (-> state
       (update-in [:registers reg-a]
               (fnil * 0)
               (reg-get registers reg-b))
       (update :mul-count inc))))

(defn do-jnz [{:keys [registers pointer] :as state}
              {:keys [reg-a reg-b]}]
  (let [reg-val (reg-get registers reg-a)]
    (if (zero? reg-val)
      (inc-ptr state)
      (update state :pointer + (reg-get registers reg-b))
      )))

(defn step [state inst]
  (case (:cmd inst)
    "set" (do-set state inst)
    "sub" (do-sub state inst)
    "mul" (do-mul state inst)
    "jnz" (do-jnz state inst)
    (throw  (ex-info "unknown command"
                     {:state state
                      :inst inst}))))

;; state

(def initial-state {:registers {}
                    :pointer 0
                    :mul-count 0})

(defn solve [initial-state input]
  (let [insts (vec (map parse-line (s/split-lines input)))]
    (loop [s initial-state c 0]
      (if (or
           (= c 500000) ;; short circuit for 2nd part
           (neg? (:pointer s))
           (<= (count insts) (:pointer s))) s
          (recur (step s (get insts (:pointer s)))
                 (inc c))))))

(comment
  (:mul-count (solve initial-state input-raw))
  (solve (assoc-in initial-state [:registers (symbol "a")] 1) input-raw)
  ;; 109300 126300
  )


;;part 2

(defn prime? [n]
  (and (odd? n)
       (let [root (int (Math/sqrt n))]
         (loop [i 3]
           (or (> i root)
               (and (not (zero? (mod n i)))
                    (recur (+ i 2))))))))
(defn prime? [n]
  (and
   (odd? n)
   (> n 3)
   (not-any? #(zero? (mod n %))
             (range 3 (inc (Math/sqrt n)) 2))))

(comment
  (count (remove prime? (range 109300 126301 17)))
  )
