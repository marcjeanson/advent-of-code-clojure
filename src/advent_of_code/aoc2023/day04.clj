(ns advent-of-code.aoc2023.day04
  (:require
   [clojure.string :as str]
   [clojure.set :as set]
   [clojure.java.io :as io]))

(def input (line-seq (io/reader (io/resource "aoc2023/day04.txt"))))

(defn card-matches [input]
  (let [[id winning-str mine-str] (rest (re-find #"Card\s+(\d+): ([\d\s]+) \| ([\d\s]+)" input))
        winning (into #{} (map parse-long (str/split (str/trim winning-str) #"\s+")))
        mine (into #{} (map parse-long (str/split (str/trim mine-str) #"\s+")))
        matches (count (set/intersection winning mine))]
    [(parse-long id) matches]))

(defn card-scores [input]
  (->> input
       (reduce (fn [acc card]
                 (conj acc (card-matches card))) {})))

(defn part1 [input]
  (->> (card-scores input)
       vals
       (map #(int (Math/pow 2 (dec %))))
       (reduce +)))

(defn winning-cards [scores id]
  (let [wins (get scores id)]
    (range (inc id) (inc (+ id wins)))))

(defn part2 [input]
    (let [scores (card-scores input)]
      (loop [cnt 0
             coll (keys scores)]
        (if (empty? coll)
          cnt
          (let [first-item (first coll)
                remaining (rest coll)
                winners (winning-cards scores first-item)]
            (recur (inc cnt) (reduce conj remaining winners)))))))

(comment

  (def test-data ["Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
                  "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
                  "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
                  "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
                  "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
                  "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"])

  (part1 test-data) ;; => 13

  (part1 input) ;; => 26426

  (part2 test-data) ;; => 30

  (part2 input) ;; => 6227972


  )
