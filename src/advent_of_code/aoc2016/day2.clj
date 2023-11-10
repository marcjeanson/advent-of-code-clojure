(ns advent-of-code.aoc2016.day2
  (:require
   [clojure.java.io :as io]))

(def input (line-seq (io/reader (io/resource "aoc2016/day02.txt"))))

(def keypad-1 [[1 2 3]
             [4 5 6]
             [7 8 9]])

(defn move-1 [[x y] direction]
  (let [delta (case direction
                \U [-1 0]
                \D [1 0]
                \L [0 -1]
                \R [0 1])]
    (->> [x y]
         (mapv + delta)
         (mapv #(min (max % 0) 2)))))

(defn process-line-1 [position line]
  (reduce move-1 position line))

(defn part-1 [start input]
  (reduce (fn [acc line]
            (let [result (process-line-1 (:position acc) line)
                  button (get-in keypad-1 result)]
              (-> acc
                  (assoc :position result)
                  (update :pin conj button)))) {:position start :pin []} input))

;; ------------------------------

(def keypad-2 {0 {2 1}
               1 {1 2 2 3 3 4}
               2 {0 5 1 6 2 7 3 8 4 9}
               3 {1 \A 2 \B 3 \C}
               4 {2 \D}})

(defn move-2 [curr direction]
  (let [delta (case direction
                \U [-1 0]
                \D [1 0]
                \L [0 -1]
                \R [0 1])
        new-button (mapv + curr delta)]
    (if (get-in keypad-2 new-button)
      new-button
      curr)))

(defn process-line-2 [position line]
  (reduce move-2 position line))

(defn part-2 [start input]
  (reduce (fn [acc line]
            (let [result (process-line-2 (:position acc) line)
                  button (get-in keypad-2 result)]
              (-> acc
                  (assoc :position result)
                  (update :pin conj button)))) {:position start :pin []} input))

(comment
  (def test-input ["ULL" "RRDDD" "LURDL" "UUUUD"])

  (part-1 [1 1] test-input)
  (part-1 [1 1] input)

  (part-2 [2 0] test-input)
  (part-2 [2 0] input)

  )
