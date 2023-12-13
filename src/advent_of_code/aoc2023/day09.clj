(ns advent-of-code.aoc2023.day09
  (:require
   [clojure.java.io :as io]))


(def input (line-seq (io/reader (io/resource "aoc2023/day09.txt"))))

(defn parse-history [s]
  (->> (re-seq #"[-\d]+" s)
       (mapv parse-long)))

(defn diff-sequence [history]
  (->> (partition 2 1 history)
       (mapv (fn [[a b]] (- b a)))))

(defn not-all-zeros? [v]
  (some (complement zero?) v))

(defn next-history-value [history]
  (let [pre (take-while not-all-zeros? (iterate diff-sequence history))
        sequences (reverse pre)]
    (->> sequences
         (map peek)
         (reduce +))))

(defn part1 [input]
  (let [histories (->> input
                       (map parse-history))]
    (->> histories
         (map next-history-value)
         (reduce +))))

(defn next-history-value2 [history]
  (let [pre (take-while not-all-zeros? (iterate diff-sequence history))
        sequences (reverse pre)]
    (->> sequences
         (map first)
         (reduce (fn [acc x]
                   (- x acc))))))

(defn part2 [input]
  (let [histories (->> input
                       (map parse-history))]
    (->> histories
         (map next-history-value2)
         (reduce +))))

(comment

  (def test-data ["0 3 6 9 12 15"
                  "1 3 6 10 15 21"
                  "10 13 16 21 30 45"])

  (part1 test-data) ;; => 114

  (part1 input) ;; => 2174807968

  (part2 test-data) ;; => 2

  (part2 input) ;; => 1208


  )
