(ns aoc2021.day04
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]))

(def input (line-seq (io/reader (io/resource "aoc2021/day04.txt"))))

(def test-nums-str "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1")
(def all-boards [
             [22 13 17 11  0]
             [ 8  2 23  4 24]
             [21  9 14 16  7]
             [ 6 10  3 18  5]
             [ 1 12 20 15 19]

             [ 3 15  0  2 22]
             [ 9 18 13 17  5]
             [19  8  7 25 23]
             [20 11 10 24  4]
             [14 21 16 12  6]

             [14 21 17 24  4]
             [10 16 15  9 19]
             [18  8 23 26 20]
             [22 11 13  6  5]
             [ 2  0 12  3  7]])

(defn str-to-int-list [s]
  (->> (mapv #(Integer/parseInt %) (str/split s #","))
       reverse
       (into ())))

(defn str-to-int-vec [s]
  (mapv #(Integer/parseInt %) (str/split (str/trim s) #"\ +")))

(defn board-bingo? [board drawn]
  (let [rotated (apply mapv vector board)]
    (or
     (some #(every? drawn %) board)
     (some #(every? drawn %) rotated))))

(defn calc-board-score [board nums]
  (let [drawn (set nums)
        board-set (fn [b]
                    (reduce #(set/union %1 %2) #{} (map set b)))]
    (when (board-bingo? board drawn)
      (* (peek nums)
         (apply + (set/difference (board-set board) drawn))))))

(defn calc-all-board-scores [boards drawn]
  (mapv #(calc-board-score % drawn) (partition 5 boards)))

(defn win-bingo [nums boards]
  (loop [drawn (list (peek nums))
         draw (pop nums)]
    (let [winning-score (first (filter (complement nil?) (calc-all-board-scores boards drawn)))]
      (if winning-score
        winning-score
        (recur (conj drawn (peek draw)) (pop draw))))))

(defn lose-bingo [nums boards]
  (loop [drawn (list (peek nums))
         draw (pop nums)
         all-scores (calc-all-board-scores boards drawn)]
    (let [new-scores (calc-all-board-scores boards drawn)]
      (if (not-any? nil? new-scores)
        (get new-scores (.indexOf all-scores nil))
        (recur (conj drawn (peek draw)) (pop draw) new-scores)))))

(comment
  ; part 1
  (let [nums (str-to-int-list (first input))
        all-boards (mapv #(str-to-int-vec %) (filter (complement empty?) (rest input)))]
    (win-bingo nums all-boards)
    )

  ; part 2
  (let [nums (str-to-int-list (first input))
        all-boards (mapv #(str-to-int-vec %) (filter (complement empty?) (rest input)))]
    (lose-bingo nums all-boards)
    )
  )
