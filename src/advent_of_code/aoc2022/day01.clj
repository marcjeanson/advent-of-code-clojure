(ns advent-of-code.aoc2022.day01
  (:require
   [clojure.java.io :as io]))

(def input (line-seq (io/reader (io/resource "aoc2022/day01.txt"))))

(defn part1 [input]
  (->> input
       (partition-by #(= "" %))
       (map (fn [calories-list]
              (->> calories-list
                   (map parse-long)
                   (reduce +)
                   )))
       (filter identity)
       (apply max)))


(defn part2 [input]
  (->> input
       (partition-by #(= "" %))
       (map (fn [calories-list]
              (->> calories-list
                   (map parse-long)
                   (reduce +)
                   )))
       (filter identity)
       sort
       reverse
       (take 3)
       (reduce +)))

(comment

  (def test-data ["1000"
                  "2000"
                  "3000"
                  ""
                  "4000"
                  ""
                  "5000"
                  "6000"
                  ""
                  "7000"
                  "8000"
                  "9000"
                  ""
                  "10000"])

  (part1 test-data) ;; => 24000
  (part1 input) ;; => 68467

  (part2 test-data) ;; => 45000
  (part2 input) ;; => 203420

  )
