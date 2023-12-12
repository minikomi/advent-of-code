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

(def number-group
  ~(group (some (* (number :d+) (? " ")))))

(def seeds-line
  ~(* "seeds: " ,number-group))

(test (peg/match seeds-line "seeds: 1 2 3\n") @[@[1 2 3]])

(def map-block
  ~(group (* (<- :w+) "-to-" (<- :w+) " map:\n"
             (some (* ,number-group (? "\n")))
             (? "\n"))))

(test (peg/match map-block "seed-to-soil map:\n1 2 3\n4 5 6\n\n")
      @[@["seed" "soil" @[1 2 3] @[4 5 6]]])

(def parser
  (peg/compile ~(* ,seeds-line "\n\n" (some ,map-block))))

(test (peg/match parser input1)
      @[@[79 14 55 13]
        @["seed" "soil" @[50 98 2] @[52 50 48]]
        @["soil" "fertilizer" @[0 15 37] @[37 52 2] @[39 0 15]]
        @["fertilizer" "water" @[49 53 8] @[0 11 42] @[42 0 7] @[57 7 4]]
        @["water" "light" @[88 18 7] @[18 25 70]]
        @["light" "temperature" @[45 77 23] @[81 45 19] @[68 64 13]]
        @["temperature" "humidity" @[0 69 1] @[1 0 69]]
        @["humidity" "location" @[60 56 37] @[56 93 4]]])

(defn between [a b x]
  (and (<= a x) (< x b)))

(defn get-mapping [seed [from to & mappings]]
  (let
    [dest (util/loopv [[dest src width] :in mappings
                       :when (between src (+ src width) seed)]
                      (+ dest (- seed src)))]
    (or dest seed)))

(test
  (let [[_ & mapping-data] (peg/match parser input1)
        mappings (mapping-data 0)]
    (map |(tuple $0 (get-mapping $0 mappings)) [98 99 50 97 10]))
  @[[98 50]
    [99 51]
    [50 52]
    [97 99]
    [10 10]])

(defn get-locations-seq-for-seed [seed mapping-data]
  (var loc seed)
  (seq [m :in mapping-data]
    (set loc (get-mapping loc m))))

(defn plot-all-mappings [seeds mapping-data]
  (map |(get-locations-seq-for-seed $0 mapping-data) seeds))

(defn solve [input]
  (let [[seeds & mapping-data] (peg/match parser input)]
    (->> (plot-all-mappings seeds mapping-data)
         (map last)
         (apply min))))

(test (solve input1) 35)

(def day5-input (util/read-file "./resources/day5.txt"))

(test (solve day5-input) 175622908)
