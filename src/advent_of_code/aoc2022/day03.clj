(ns advent-of-code.aoc2022.day03
  (:require
   [clojure.set :as set]
   [clojure.java.io :as io]))

(def input (line-seq (io/reader (io/resource "aoc2022/day03.txt"))))


(defn item [input]
  (let [mid (-> input count (quot 2))
        [comp1 comp2] (split-at mid input)
        comp1-set (set comp1)
        comp2-set (set comp2)]
    (first (set/intersection comp1-set comp2-set))))

(defn priority [item]
  (let [cap? (< (- (int item) (int \A)) 26)]

    (if cap?
      (+ 27 (- (int item) (int \A)))
      (+ 1 (- (int item) (int \a))))))

(defn part1 [input]
  (->> input
       (map item)
       (map priority)
       (reduce +)))

(defn part2 [input]
  (->> input
       (partition 3)
       (map (fn [group]
              (let [sacks (map set group)]
                (first (set/intersection (first sacks) (second sacks) (last sacks))))))
       (map priority)
       (reduce +)))

(comment
  (def test-data ["vJrwpWtwJgWrhcsFMMfFFhFp"
                  "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
                  "PmmdzqPrVvPwwTWBwg"
                  "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
                  "ttgJtRGJQctTZtZT"
                  "CrZsJsPPZsGzwwsLwLmpwMDw"])

  (part1 test-data) ;; => 157

  (part1 input) ;; => 8394

  (part2 test-data) ;; => 70

  (part2 input) ;; => 2413

  )
