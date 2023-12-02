(ns advent-of-code.aoc2022.day04
  (:require
   [clojure.set :as set]
   [clojure.java.io :as io]))

(def input (line-seq (io/reader (io/resource "aoc2022/day04.txt"))))

(defn fully-contained? [[a b c d]]
  (let [section1 (into #{} (range a (inc b)))
        section2 (into #{} (range c (inc d)))]
    (or (set/subset? section1 section2) (set/subset? section2 section1))))

(defn part1 [input]
  (->> input
       (map (fn [pair]
              (->> pair
                   (re-find #"(\d+)-(\d+),(\d+)-(\d+)")
                   rest
                   (mapv parse-long)
                   (fully-contained?))))
       (filter true?)
       count))

(defn overlapped? [[a b c d]]
  (let [section1 (into #{} (range a (inc b)))
        section2 (into #{} (range c (inc d)))]
    (not-empty (set/intersection section1 section2))))

(defn part2 [input]
  (->> input
       (map (fn [pair]
              (->> pair
                   (re-find #"(\d+)-(\d+),(\d+)-(\d+)")
                   rest
                   (mapv parse-long)
                   (overlapped?))))
       (filter identity)
       count))

(comment

  (def test-data ["2-4,6-8"
                  "2-3,4-5"
                  "5-7,7-9"
                  "2-8,3-7"
                  "6-6,4-6"
                  "2-6,4-8"])


  (part1 test-data) ;; => 2

  (part1 input) ;; => 538

  (part2 test-data) ;; => 4

  (part2 input) ;; => 792

  )
