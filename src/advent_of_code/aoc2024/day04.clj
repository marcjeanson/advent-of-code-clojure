(ns advent-of-code.aoc2024.day04
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (line-seq (io/reader (io/resource "aoc2024/day04.txt"))))

(defn part1 [input]
  (let [grid (mapv vec input)
        rows (count grid)
        cols (count (first grid))
        offsets [[[0 -1]  [0 -2]  [0 -3]]
                 [[-1 -1] [-2 -2] [-3 -3]]
                 [[-1 0] [-2 0] [-3 0]]
                 [[-1 1] [-2 2] [-3 3]]
                 [[0 1] [0 2] [0 3]]
                 [[1 1] [2 2] [3 3]]
                 [[1 0] [2 0] [3 0]]
                 [[1 -1] [2 -2] [3 -3]]]
        at (fn [[x y] [a b]] (get-in grid [(+ x a) (+ y b)]))]

    (count
     (flatten
      (for [x (range rows)
           y (range cols)
           :let [curr (get-in grid [x y])]
           :when (= curr \X)]

       (->> offsets
            (mapv (fn [row]
                    (let [arounds (mapv (fn [[a b]]
                                          (at [x y] [a b])) row)]
                      (apply str arounds))))

            (filterv #(= "MAS" %))))))))


(defn part2 [input]
  (let [grid (mapv vec input)
        rows (count grid)
        cols (count (first grid))
        offsets [[[-1 -1] [1 1]]
                 [[-1 1] [1 -1]]]
        at (fn [[x y] [a b]] (get-in grid [(+ x a) (+ y b)]))]

    (->> (for [x (range rows)
               y (range cols)
               :let [curr (get-in grid [x y])]
               :when (= curr \A)]

           (->> offsets
                (mapv (fn [row]
                        (let [arounds (mapv (fn [[a b]]
                                              (at [x y] [a b])) row)]
                          (= #{\M \S} (set arounds)))))
                (every? true?)))
         (filter true?)
         count)))

(comment

  (def test-input '(
                    "MMMSXXMASM"
                    "MSAMXMSMSA"
                    "AMXSXMAAMM"
                    "MSAMASMSMX"
                    "XMASAMXAMM"
                    "XXAMMXXAMA"
                    "SMSMSASXSS"
                    "SAXAMASAAA"
                    "MAMMMXMMMM"
                    "MXMXAXMASX"))

  (part1 test-input)
  (part2 test-input)

  (part1 input) ;; => 2414
  (part2 input) ;; => 1871

  )
