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

(defn reg-get [registers r]
  (if (number? r) r
      (get registers r 0)))

(defn do-snd [{:keys [registers] :as state}
              {:keys [reg-a]}]
  (inc-ptr (assoc state :last (get registers reg-a 0))))

(defn do-set [{:keys [registers] :as state}
              {:keys [reg-a reg-b]}]
  (inc-ptr
   (update state :registers
           assoc
           reg-a
           (reg-get registers reg-b))))

(defn do-add [{:keys [registers] :as state}
              {:keys [reg-a reg-b]}]
  (inc-ptr
   (update-in state [:registers reg-a]
              (fnil + 0)
              (reg-get registers reg-b))))

(defn do-mul [{:keys [registers] :as state}
              {:keys [reg-a reg-b]}]
  (inc-ptr
   (update-in state [:registers reg-a]
              (fnil * 0)
              (reg-get registers reg-b))))

(defn do-mod [{:keys [registers] :as state}
              {:keys [reg-a reg-b]}]
  (inc-ptr
   (update-in state [:registers reg-a]
              (fnil mod 0)
              (reg-get registers reg-b))))

(defn do-jgz [{:keys [registers pointer] :as state}
              {:keys [reg-a reg-b]}]
  (let [reg-val (reg-get registers reg-a)]
    (if (pos? reg-val)
      (update state :pointer + (reg-get registers reg-b))
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

(defn solve1 [input]
  (let [insts (vec (map parse-line (s/split-lines input)))]
    (loop [s initial-state]
      (if (= "rcv" (:cmd (get insts (:pointer s)))) s
          (recur (step s (get insts (:pointer s))))))))

;; part 2
;; -----------------------------------------------------

(def initial-state-a
  {:registers {'p 0}
   :program-name 1
   :pointer 0
   :sent 0
   :queue (clojure.lang.PersistentQueue/EMPTY)})

(def initial-state-b
  {:registers {'p 1}
   :program-name 2
   :pointer 0
   :sent 0
   :queue (clojure.lang.PersistentQueue/EMPTY)})

(def test-input2 "snd 1
snd 2
snd p
rcv a
rcv b
rcv c
rcv d")

(defn do-snd2 [{:keys [registers] :as working} other {:keys [reg-a]}]
  [(-> working
       (update :sent inc)
       (update :pointer inc))
   (update other :queue
           conj
           (reg-get registers reg-a))])

(defn do-rcv [insts working other cmd]
  (if-let [p (peek (:queue working))]
    ;; still have items in queue
    [(-> working
         (assoc-in [:registers (:reg-a cmd)] p)
         (update :pointer inc)
         (update :queue pop))
     other]
    ;; check state of other machine
    (let [o-cmd (get insts (:pointer other))]
      (if (and (empty? (:queue other))
               (= (:cmd o-cmd) "rcv"))
        ;; deadlock
        false
        ;; swap working process
        [other working]))))

(defn solve2 [input]
  (let [insts (vec (map parse-line (s/split-lines input)))]
    (loop [working initial-state-a
           other   initial-state-b]
      (let [w-cmd (get insts (:pointer working))]
        (case (:cmd w-cmd)
          "rcv" (if-let [[*w *o] (do-rcv insts working other w-cmd)]
                  (recur *w *o)
                  [working other])
          "snd" (let [[w* o*] (do-snd2 working other w-cmd)]
                  (recur w* o*))
          (recur (step working w-cmd) other))))))

(comment (clojure.pprint/pprint (solve2 input)))
