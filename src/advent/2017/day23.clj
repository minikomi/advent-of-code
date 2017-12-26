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

(def initial-state {:registers {
                                `a 0
                                `b 0
                                `c 0
                                `d 0
                                `e 0
                                `f 0
                                `g 0
                                `h 0
                                }
                    :pointer 0
                    :mul-count 0})

(defn solve1 [input]
  (let [insts (vec (map parse-line (s/split-lines input)))]
    (loop [s initial-state]
      (if (or
           (neg? (:pointer s))
           (<= (count insts) (:pointer s))) s
          (recur (step s (get insts (:pointer s))))))))

(comment
  (solve1 input-raw)
  )
