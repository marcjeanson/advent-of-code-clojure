(ns aoc2021.day05
  (:require [clojure.java.io :as io]))

(def input (line-seq (io/reader (io/resource "aoc2021/day05.txt"))))

(def test-data '("0,9 -> 5,9"
                 "8,0 -> 0,8"
                 "9,4 -> 3,4"
                 "2,2 -> 2,1"
                 "7,0 -> 7,4"
                 "6,4 -> 2,0"
                 "0,9 -> 2,9"
                 "3,4 -> 1,4"
                 "0,0 -> 8,8"
                 "5,5 -> 8,2"))

(defn str-to-points
  [s]
  (->> s
       (re-matches #"(\d+),(\d+) -> (\d+),(\d+)")
       rest
       (mapv #(Integer/parseInt %))))

(defn filter-hv-lines [data]
  (->> data
       (map #(str-to-points %))
       (filter (fn [[x1 y1 x2 y2]]
                 (or
                  (= x1 x2)
                  (= y1 y2))))))

(defn points [data]
  (map (fn [coords]
         (let [[x1 y1 x2 y2] coords
               x-range (vec (if (> x1 x2)
                              (reverse (range x2 (inc x1)))
                              (range x1 (inc x2))))
               y-range (vec (if (> y1 y2)
                              (reverse (range y2 (inc y1)))
                              (range y1 (inc y2))))
               size (max (count x-range) (count y-range))]
           (for [t (range size)]
             [(get x-range t (first x-range)) (get y-range t (first y-range))]))) data))

(comment
  ; part 1
  (->> test-data
       filter-hv-lines
       points
       (apply concat)
       frequencies
       vals
       (filter #(>= % 2))
       count)

  (->> input
       filter-hv-lines
       points
       (apply concat)
       frequencies
       vals
       (filter #(>= % 2))
       count)

  ; part 2
  (->> test-data
       (map #(str-to-points %))
       points
       (apply concat)
       frequencies
       vals
       (filter #(>= % 2))
       count)

  (->> input
       (map #(str-to-points %))
       points
       (apply concat)
       frequencies
       vals
       (filter #(>= % 2))
       count)

  )
