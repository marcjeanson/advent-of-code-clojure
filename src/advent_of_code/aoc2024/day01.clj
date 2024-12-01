(ns advent-of-code.aoc2024.day01
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (line-seq (io/reader (io/resource "aoc2024/day01.txt"))))

(defn part1 [input]
  (let [input (->> input (map (fn [line] (str/split line #"\ +"))))
        left-list (sort (map #(-> % first (parse-long)) input))
        right-list (sort (map #(-> % last parse-long) input))
        distances (map #(abs (- %1 %2)) left-list right-list) ]
    (reduce + distances)))

(defn part2 [input]
  (let [input (->> input (map (fn [line] (str/split line #"\ +"))))
        left-list (map #(-> % first (parse-long)) input)
        right-list (map #(-> % last parse-long) input)
        freqs (frequencies right-list)]

    (->> left-list
         (map #(* % (get freqs % 0)))
         (reduce +))))

(comment
  (def test-input
    '("3   4"
      "4   3"
      "2   5"
      "1   3"
      "3   9"
      "3   3"))

  (part1 input)
  (part2 input)

  )
