(ns advent-of-code.aoc2016.day6
  (:require
   [clojure.java.io :as io]))

(def input (line-seq (io/reader (io/resource "aoc2016/day06.txt"))))

(defn part-1 [input]
  (->> input
       (apply mapv vector)
       (mapv #(frequencies %))
       (mapv #(sort-by val %))
       (mapv last)
       keys
       (apply str)))

(defn part-2 [input]
  (->> input
       (apply mapv vector)
       (mapv #(frequencies %))
       (mapv #(sort-by val %))
       (mapv first)
       keys
       (apply str)))

(comment

  (def sample ["eedadn"
               "drvtee"
               "eandsr"
               "raavrd"
               "atevrs"
               "tsrnev"
               "sdttsa"
               "rasrtv"
               "nssdts"
               "ntnada"
               "svetve"
               "tesnvt"
               "vntsnd"
               "vrdear"
               "dvrsen"
               "enarar"])


  (part-1 sample)

  (part-1 input)
  ;; => "gebzfnbt"

  (part-2 sample)
  ;; => "advent"
  (part-2 input)
  ;; => "fykjtwyn"

  )
