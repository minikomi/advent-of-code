(import ./util :fresh true)
(use judge)

(def day7-input (util/read-file "./resources/day7.txt"))

(def input1
  `32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483`)

(def cards "23456789TJQKA")

(def card-score
  (tabseq [i :range [0 (length cards)]]
    (get cards i) i))

(test (map card-score cards) @[0 1 2 3 4 5 6 7 8 9 10 11 12])

(def parser ~{:card (set ,cards)
              :cards (<- (some :card))
              :bid (number :d+)
              :line (group (* :cards " " :bid (? "\n")))
              :main (some :line)})

(test (peg/match parser input1)
      @[@["32T3K" 765]
        @["T55J5" 684]
        @["KK677" 28]
        @["KTJJT" 220]
        @["QQQJA" 483]])

(def hand-score {:five-of-a-kind 6
                 :four-of-a-kind 5
                 :full-house 4
                 :three-of-a-kind 3
                 :two-pair 2
                 :pair 1
                 :card-high 0})

(defn hand-value [hand]
  (let [card-vals (sort (values (frequencies hand)) >)]
    (match card-vals
      [5] :five-of-a-kind
      [4 1] :four-of-a-kind
      [3 2] :full-house
      [3 1 1] :three-of-a-kind
      [2 2 1] :two-pair
      [2 1 1 1] :pair
      _ :card-high)))

(test (let [[hand bid] (first (peg/match parser input1))]
        [hand (hand-value hand)]) ["32T3K" :pair])

(defn parse-hand [[hand bid]]
  (let [hv (hand-value hand)]
    {:card-scores (map card-score hand)
     :hand-type hv
     :hand-score (hand-score hv)
     :bid bid}))

(test (map parse-hand (peg/match parser input1))
      @[{:bid 765
         :card-scores @[1 0 8 1 11]
         :hand-score 1
         :hand-type :pair}
        {:bid 684
         :card-scores @[8 3 3 9 3]
         :hand-score 3
         :hand-type :three-of-a-kind}
        {:bid 28
         :card-scores @[11 11 4 5 5]
         :hand-score 2
         :hand-type :two-pair}
        {:bid 220
         :card-scores @[11 8 9 9 8]
         :hand-score 2
         :hand-type :two-pair}
        {:bid 483
         :card-scores @[10 10 10 9 12]
         :hand-score 3
         :hand-type :three-of-a-kind}])

(defn sort-hands [hands]
  (sorted-by |(tuple ($0 :hand-score) ;($0 :card-scores)) hands))

(test (let [parsed (map parse-hand (peg/match parser input1))]
        (sort-hands parsed))
      @[{:bid 765
         :card-scores @[1 0 8 1 11]
         :hand-score 1
         :hand-type :pair}
        {:bid 220
         :card-scores @[11 8 9 9 8]
         :hand-score 2
         :hand-type :two-pair}
        {:bid 28
         :card-scores @[11 11 4 5 5]
         :hand-score 2
         :hand-type :two-pair}
        {:bid 684
         :card-scores @[8 3 3 9 3]
         :hand-score 3
         :hand-type :three-of-a-kind}
        {:bid 483
         :card-scores @[10 10 10 9 12]
         :hand-score 3
         :hand-type :three-of-a-kind}])

(defn solve [input]
  (def sorted-hands
    (->> input
         (peg/match parser)
         (map parse-hand)
         sort-hands))
  (var v 0)
  (def scores (seq [h :in sorted-hands] (++ v) (* v (h :bid))))
  (sum scores))

(test (solve input1) 6440)


(test (solve day7-input) 250898830)

# part 2
# ------

(def joker-cards "J23456789TQKA")

(def joker-card-score
  (tabseq [i :range [0 (length joker-cards)]]
    (get joker-cards i) i))

(defn joker-card-vals [hand]
  (let [non-j
        (sort
          (->> hand
               (filter |(not= ("J" 0) $0))
               frequencies
               values) >)]
    (if (empty? non-j)
      [5]
      (update non-j 0 + (- 5 (sum non-j))))))

(defn joker-hand-value [hand]
  (let [card-vals (joker-card-vals hand)]
    (match card-vals
      [5] :five-of-a-kind
      [4 1] :four-of-a-kind
      [3 2] :full-house
      [3 1 1] :three-of-a-kind
      [2 2 1] :two-pair
      [2 1 1 1] :pair
      _ :card-high)))

(test (joker-hand-value "QJJQ2") :four-of-a-kind)
(test (joker-hand-value "JJJJJ") :five-of-a-kind)

(defn joker-parse-hand [[hand bid]]
  (let [hv (joker-hand-value hand)]
    {:card-scores (map joker-card-score hand)
     :hand-type hv
     :hand-score (hand-score hv)
     :bid bid}))

(test (joker-parse-hand ["JKKK2" 1])
      {:bid 1
       :card-scores @[0 11 11 11 1]
       :hand-score 5
       :hand-type :four-of-a-kind})

(test (joker-parse-hand ["QQJQ2" 1])
      {:bid 1
       :card-scores @[10 10 0 10 1]
       :hand-score 5
       :hand-type :four-of-a-kind})

(defn solve2 [input]
  (def sorted-hands
    (->> input
         (peg/match parser)
         (map joker-parse-hand)
         sort-hands))
  (var v 0)
  (def scores (seq [h :in sorted-hands] (++ v) (* v (h :bid))))
  (sum scores))

(map joker-parse-hand (peg/match parser day7-input))

(test (solve2 input1) 5905)
(test (solve2 day7-input) 252127335)
