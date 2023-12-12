(import ./util :fresh true)
(use judge)


(def input1
`seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4`)

(def seeds-line
    (peg/compile
        )))

(test (peg/match seeds-line "seeds: 1 2 3\n") @[1 2 3])

(def map-block
  (peg/compile
    ~))

(test (peg/match map-block "seed-to-soil map:\n1 2 3\n4 5 6\n\n"))

(def parse-input
  ~{:number-group (group (some (* (number :d+) (? " "))))
    :seeds (* "seeds: " :number-group)
    :map-block (group (* (<- :w+) "-to-" (<- :w+) " map:\n"
        (some (* :number-group (? "\n")))
        (? "\n")))
    :main (* :seeds "\n\n" (some :map-block))})

(def [initial-seeds
      & maps] (peg/match parse-input input1))

(defn build-mapping [[_ _ & mappings]]
  (pp mappings)
  (tabseq [[dest src n] :in mappings
           delta :range [0 n]
           ]
     (+ src delta) (+ dest delta)))

((build-mapping ((peg/match parse-input input1) 1)) 98)
