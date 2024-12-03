(ns advent-of-code.aoc2024.day02
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (line-seq (io/reader (io/resource "aoc2024/day02.txt"))))

(defn lines-to-numbers [input]
  (->> input
       (mapv (fn [line]
              (let [line (str/split line #"\ +")]
                (mapv #(parse-long %) line))))))

(defn safe? [report]
  (let [level-diffs (->> report (partition 2 1) (map #(apply - %)))
        increasing? (every? pos? level-diffs)
        decreasing? (every? neg? level-diffs)
        allowed-diff? (every? (fn [level-diff]
                                (let [x (abs level-diff)]
                                  (and (>= x 1) (<= x 3)))) level-diffs)]
    (and (or increasing? decreasing?) allowed-diff?)))

(defn part1 [input]
  (->> input
       (map safe?)
       (filter true?)
       count))

(defn safe-with-dampener? [report]
  (let [reports (map-indexed
                 (fn [idx _]
                   (concat (take idx report) (drop (inc idx) report)))
                 report)]
    (some safe? reports)))

(defn part2 [input]
  (->> input
       (map #(or (safe? %) (safe-with-dampener? %)))
       (filter true?)
       count))

(comment

  (part1 (lines-to-numbers input)) ;; => 432

  (def test-input
    [
     [ 7 6 4 2 1 ]
     [ 1 2 7 8 9 ]
     [ 9 7 6 2 1 ]
     [ 1 3 2 4 5 ]
     [ 8 6 4 4 1 ]
     [ 1 3 6 7 9 ]
     ]
    )

  (part1 test-input) ;; => 2


  (safe-with-dampener? [ 1 3 2 4 5 ])
  (part2 test-input)

  (part2 (lines-to-numbers input)) ;; => 488



  )
