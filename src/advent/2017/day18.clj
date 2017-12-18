(ns advent.2017.day18
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [advent.util :as util]))

(def test-input "set a 1
add a 2
mul a a
mod a 5
snd a
set a 0
rcv a
jgz a -1
set a 1
jgz a -2")

(def input
  "set i 31
set a 1
mul p 17
jgz p p
mul a 2
add i -1
jgz i -2
add a -1
set i 127
set p 735
mul p 8505
mod p a
mul p 129749
add p 12345
mod p a
set b p
mod b 10000
snd b
add i -1
jgz i -9
jgz a 3
rcv b
jgz b -1
set f 0
set i 126
rcv a
rcv b
set p a
mul p -1
add p b
jgz p 4
snd a
set a b
jgz 1 3
snd b
set f 1
add i -1
jgz i -11
snd a
jgz f -16
jgz a -19")

(defn parse-line [ln]
  (let [wrds (s/split ln #" ")]
    {:cmd (first wrds)
     :reg-a (read-string (second wrds))
     :reg-b (when-let [w (get wrds 2)]
              (read-string w))}))

(defn inc-ptr [state]
  (update state :pointer inc))

(defn do-snd [{:keys [registers] :as state}
              {:keys [reg-a]}]
  (inc-ptr (assoc state :last (get registers reg-a 0))))

(defn do-set [{:keys [registers] :as state}
              {:keys [reg-a reg-b]}]
  (inc-ptr (update state :registers
           assoc
           reg-a
           (if (number? reg-b)
             reg-b
             (get registers reg-b 0)))))

(defn do-add [{:keys [registers] :as state}
              {:keys [reg-a reg-b]}]
  (inc-ptr
   (update-in state [:registers reg-a]
              (fnil + 0)
              (if (number? reg-b)
                reg-b
                (get registers reg-b 0)))))

(defn do-mul [{:keys [registers] :as state}
              {:keys [reg-a reg-b]}]
  (inc-ptr
   (update-in state [:registers reg-a]
              (fnil * 0)
              (if (number? reg-b)
                reg-b
                (get registers reg-b 0)))
   ))

(defn do-mod [{:keys [registers] :as state}
              {:keys [reg-a reg-b]}]
  (inc-ptr
   (update-in state [:registers reg-a]
              (fnil mod 0)
              (if (number? reg-b)
                reg-b
                (get registers reg-b 0)))))

(defn do-jgz [{:keys [registers pointer] :as state}
              {:keys [reg-a reg-b]}]
  (let [reg-val (get registers reg-a 0)]
    (if (pos? reg-val)
      (update state :pointer
              +
              (if (number? reg-b)
                reg-b
                (get registers reg-b 0)))
      (inc-ptr state))))

(defn step [state inst]
  (case (:cmd inst)
    "snd" (do-snd state inst)
    "set" (do-set state inst)
    "add" (do-add state inst)
    "mul" (do-mul state inst)
    "mod" (do-mod state inst)
    "jgz" (do-jgz state inst)
    (throw  (ex-info "unknown command"
                     {:state state
                      :inst inst}))))

(def initial-state {:registers {} :pointer 0})

(comment
  (do-add
   (do-set initial-state (parse-line "set a 1"))
   (parse-line "add a 2"))
  (let [insts (vec (map parse-line (s/split-lines input)))]
    (loop [s initial-state]
      (if (= "rcv" (:cmd (get insts (:pointer s)))) s
          (recur (step s (get insts (:pointer s)))))))
