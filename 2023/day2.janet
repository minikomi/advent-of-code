(import ./util)
(use judge)

(def input1 (string/split "\n"
                          `Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green`))

(def grammar1
  ~{:num-color (group
                 (* (number :d+)
                    " "
                    (<- (+ "blue" "green" "red"))))
    :num-colors (* :num-color
                   (? (some (* ", " :num-color)))
                   (? "; "))
    :main (* "Game " (number :d+) ": " (group (some :num-colors)))})

(def num-cubes @{"red" 12 "green" 13 "blue" 14})

(defn game-is-possible
  [[id cubes-pulled]]
  (var possible? true)
  (loop [[num color] :in cubes-pulled
         :let [num-cubes (get num-cubes color)]
         :when possible?]
    (set possible? (<= num num-cubes)))
  possible?)

(def day2-input (util/read-lines "./resources/day2.txt"))

(defn solve1 [lines]
  (->> lines
       (map |(peg/match grammar1 $0))
       (filter game-is-possible)
       (map first)
       sum))

(comment
  (solve1 day2-input))

(defn get-power [[id cubes-pulled]]
  (def min-required @{"green" 0 "blue" 0 "red" 0})
  (loop [[num color] :in cubes-pulled]
    (update min-required color |(max num $0)))
  (product (values min-required)))

(test
  (map get-power (map |(peg/match grammar1 $0) input1))
  @[48 12 1560 630 36])

(defn solve2 [lines]
  (->> lines
       (map |(peg/match grammar1 $0))
       (map get-power)
       sum))

(comment
  (solve2 day2-input))
