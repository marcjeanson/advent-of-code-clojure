(ns aoc2021.day01
  (:require [clojure.java.io :as io]))

(def input (map #(Long/parseLong %) (line-seq (io/reader (io/resource "aoc2021/day01.txt")))))

(def test-data [199 200 208 210 200 207 240 269 260 263])

(defn count-increasing [input]
  (->> input
       (partition 2 1)
       (filter #(apply < %))
       count))

(comment
  ; part 1
  (count-increasing test-data)
  (count-increasing input)

  ; part 2
  (->> (partition 3 1 input)
       (map #(apply + %))
       count-increasing)

  )
