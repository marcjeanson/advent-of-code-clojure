(ns advent-of-code.aoc2017.day1
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def input (slurp (io/resource "aoc2017/day01.txt")))

(defn part1 [input]
  (let [input (str/trim-newline input)
        first-char (subs input 0 1)
        input (str input first-char)
        pairs (partition 2 1 input)]

    (->> pairs
         (map (fn [[x y]]
                (if (= x y)
                  (Character/digit x 10)
                  0)))
         (reduce +))))

(defn part2 [input]
  (let [input (str/trim-newline input)
        len (count input)
        offset (quot len 2)
        pairs (->> input (map vector (take len (drop offset (cycle input)))))]
    (->> pairs
         (map (fn [[x y]]
                (if (= x y)
                  (Character/digit x 10)
                  0)))
         (reduce +))))

(comment

  (part1 input)
  (part2 input)

  )
