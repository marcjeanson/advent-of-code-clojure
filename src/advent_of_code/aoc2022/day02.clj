(ns advent-of-code.aoc2022.day02
  (:require
   [clojure.java.io :as io]))

(def input (line-seq (io/reader (io/resource "aoc2022/day02.txt"))))



(defn part1 [input]
  (let [rps {"A X" 4
             "A Y" 8
             "A Z" 3
             "B X" 1
             "B Y" 5
             "B Z" 9
             "C X" 7
             "C Y" 2
             "C Z" 6}]
    (->> input
         (map #(get rps %))
         (reduce +))))

(defn round-score [round]
  (let [rps {"A A" 4
             "A B" 8
             "A C" 3
             "B A" 1
             "B B" 5
             "B C" 9
             "C A" 7
             "C B" 2
             "C C" 6}]
    (get rps round)))


(defn round-choice [round]
  (let [rps {"A X" "A C"
             "A Y" "A A"
             "A Z" "A B"
             "B X" "B A"
             "B Y" "B B"
             "B Z" "B C"
             "C X" "C B"
             "C Y" "C C"
             "C Z" "C A"}]
    (get rps round)))

(defn part2 [input]
  (->> input
       (map round-choice)
       (map round-score)
       (reduce +)))


(comment
                                        ; A for Rock, B for Paper, and C for Scissors
                                        ; X for Rock, Y for Paper, and Z for Scissors
                                        ; The score for a single round is the score for the shape you selected (1 for Rock, 2 for Paper, and 3 for Scissors)
                                        ; plus the score for the outcome of the round (0 if you lost, 3 if the round was a draw, and 6 if you won).

  (def test-data ["A Y"
                  "B X"
                  "C Z"])

  (part1 test-data) ;; => 15

  (part1 input) ;; => 14069

  (part2 test-data) ;; => 12

  (part2 input) ;; => 12411


                                        ; part 2
  ;; X means you need to lose, Y means you need to end the round in a draw, and Z means you need to win.



  )
