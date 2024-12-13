(ns advent-of-code.aoc2024.day07
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

(def input (line-seq (io/reader (io/resource "aoc2024/day07.txt"))))

(defn parse-line [line]
  (let [numbers (->> (re-seq #"\d+" line) (map parse-long))]
    {:tv (first numbers)
     :operands (rest numbers)}))

(defn op-combos [operands]
  (let [num-of-operators (- (count operands) 1)]
    (combo/selections [+ *] num-of-operators)))

(defn calc [operands operations]
  (loop [opands (rest operands)
         ops operations
         acc (first operands)]
   (if (empty? opands)
     acc
     (recur (rest opands) (rest ops) ((first ops) acc (first opands))))))


(defn check-equation [{:keys [tv operands]}]
  (if (some (fn [ops]
              (= tv (calc operands ops)))
            (op-combos operands))
    tv
    0))

(defn total-calibration [input]
  (->> input
       (map parse-line)
       (map check-equation)
       (reduce +)))

(defn concat-nums [a b]
  (parse-long (str (str a) (str b))))

;; redefine op-combos for part 2
(defn op-combos [operands]
  (let [num-of-operators (- (count operands) 1)]
    (combo/selections [+ * concat-nums] num-of-operators)))

(comment

  (def test-input '("190: 10 19"
                    "3267: 81 40 27"
                    "83: 17 5"
                    "156: 15 6"
                    "7290: 6 8 6 15"
                    "161011: 16 10 13"
                    "192: 17 8 14"
                    "21037: 9 7 18 13"
                    "292: 11 6 16 20"))

  (total-calibration test-input) ;; => 3749
  (total-calibration input) ;; => 28730327770375

  )
