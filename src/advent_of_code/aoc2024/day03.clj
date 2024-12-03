(ns advent-of-code.aoc2024.day03
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (slurp (io/resource "aoc2024/day03.txt")))

(defn part1 [input]
  (->> (re-seq #"(mul\((\d+),(\d+)\))" input)
       (map #(drop 2 %))
       (map (fn [[x y]]
              (* (parse-long x) (parse-long y))
              ))
       (reduce +)))

(defn part2 [input]
  (let [without-junk (str/replace input #"(?s)don't\(\).*?(?:do\(\)|\Z)" "")]
    (part1 without-junk)))

(comment

  (def test-input "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")
  (part1 test-input)

  (part1 input) ;; => 188116424

  (def test-input2 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))don't()_mul(5,5)do()?mul(8,8))don't()_mul(5,5)do()?mul(9,9))")

  (part2 input) ;; => 104245808

  )
