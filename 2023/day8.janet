(import ./util :fresh true)
(use judge)

(def day8-input (util/read-file "./resources/day8.txt"))

(def input1 `RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)`)

(def input2 `LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)`)

(def input3 `LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)`)

(def grammar-instruction
  ~{:code (* :w :w :w)
    :main (* (<- :code) " = (" (group (* (<- :code) ", " (<- :code))) ")")})

(test (peg/match grammar-instruction "AAA = (BBB, CCC)")
      @["AAA" @["BBB" "CCC"]])

(def grammar ~{:rl (some (+ "L" "R"))
               :instructions (/
                               (some (* ,grammar-instruction (? "\n")))
                               ,(fn [& vs] (table ;vs)))
               :main (* (<- :rl) "\n\n"
                        :instructions)})

(test (peg/match grammar input1)
      @["RL"
        @{"AAA" @["BBB" "CCC"]
          "BBB" @["DDD" "EEE"]
          "CCC" @["ZZZ" "GGG"]
          "DDD" @["DDD" "DDD"]
          "EEE" @["EEE" "EEE"]
          "GGG" @["GGG" "GGG"]
          "ZZZ" @["ZZZ" "ZZZ"]}])

(def START "AAA")
(def STOP "ZZZ")

(defn get-sequence [[rl-seq insts]]
  (var pointer START)
  (def rl-gen (util/cycle rl-seq))
  (seq [rl :in rl-gen
        :let [inst (get insts pointer)
              next-pointer (inst ({(chr "L") 0 (chr "R") 1} rl))]
        :until (= pointer STOP)]
    (set pointer next-pointer)))

(test (get-sequence (peg/match grammar input1)) @["CCC" "ZZZ"])
(test (get-sequence (peg/match grammar input2)) @["BBB" "AAA" "BBB" "AAA" "BBB" "ZZZ"])

(test (length (get-sequence (peg/match grammar day8-input))) 18827)

(defn get-sequence-any-z [start-val [rl-seq insts]]
  (var pointer start-val)
  (def rl-gen (util/cycle rl-seq))
  (seq [rl :in rl-gen
        :let [inst (get insts pointer)
              next-pointer (inst ({(chr "L") 0 (chr "R") 1} rl))]
        :until (= (pointer 2) (chr "Z"))]
    (set pointer next-pointer)))

(test
  (get-sequence-any-z "11A" (peg/match grammar input3))
  @["11B" "11Z"])


(defn solve2 [data]
  (let [[rl-seq insts] data
        starts (filter |(string/find "A" $0) (keys insts))
        seqs (map |(get-sequence-any-z $0 data) starts)]
    (util/lcm ;(map length seqs))))

(test (solve2 (peg/match grammar input3)) 6)
(test (solve2 (peg/match grammar day8-input)) 20220305520997)
