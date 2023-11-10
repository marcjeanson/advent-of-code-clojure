(ns advent-of-code.aoc2016.day3
  (:require
   [clojure.java.io :as io]))

(def input (line-seq (io/reader (io/resource "aoc2016/day03.txt"))))

(defn is-triangle? [[a b c]]
  (and
   (> (+ a b) c)
   (> (+ a c) b)
     (> (+ b c) a)))

(defn part-1 [input]
  (->> input
       (map #(re-seq #"\d+" %))
       (mapv #(mapv (fn [i] (Integer/parseInt i)) %))
       (filter #(is-triangle? %))
       count))

(defn transpose [matrix]
  (apply mapv vector matrix))

(defn part-2 [input]
  (->> input
       (map #(re-seq #"\d+" %))
       (mapv #(mapv (fn [i] (Integer/parseInt i)) %))
       transpose
       flatten
       (partition 3)
       (filter #(is-triangle? %))
       count))

(comment
  (part-1 input)

  (part-2 input)

  )
