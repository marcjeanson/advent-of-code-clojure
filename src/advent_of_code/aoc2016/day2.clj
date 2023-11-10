(ns advent-of-code.aoc2016.day2
  (:require
   [clojure.java.io :as io]))

(def input (line-seq (io/reader (io/resource "aoc2016/day02.txt"))))

(def keypad [[1 2 3]
             [4 5 6]
             [7 8 9]])

(defn move [[x y] direction]
  (let [delta (case direction
                \U [-1 0]
                \D [1 0]
                \L [0 -1]
                \R [0 1])]
    (->> [x y]
         (mapv + delta)
         (mapv #(min (max % 0) 2)))))

(defn process-line [position line]
  (reduce move position line))

(defn part1 [input]
  (reduce (fn [acc line]
            (let [result (process-line (:position acc) line)
                  button (get-in keypad result)]
              (-> acc
                  (assoc :position result)
                  (update :pin conj button)))) {:position [1 1] :pin []} input))

(comment
  (def test-input ["ULL" "RRDDD" "LURDL" "UUUUD"])

  (part1 test-input)
  (part1 input)


  )
