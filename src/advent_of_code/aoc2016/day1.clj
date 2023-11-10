(ns advent-of-code.aoc2016.day1
  (:require
   [clojure.java.io :as io]))

(def input (slurp (io/resource "aoc2016/day01.txt")))

(defn update-direction [current turn]
  (let [directions [:north :east :south :west]
        current-index (.indexOf directions current)
        delta (if (= turn \R) 1 -1)
        new-index (mod (+ current-index delta) 4)]
    (nth directions new-index)))

(defn parse-directions [input]
  (let [matches (re-seq #"[RL]\d+" input)]
    (map (fn [match] [(first match) (Integer/parseInt (subs match 1))]) matches)))

(defn move [position direction steps]
  (let [delta (case direction
                :north [0 steps]
                :east [steps 0]
                :south [0 (- steps)]
                :west [(- steps) 0])]
    (mapv + position delta)))

(defn apply-instruction [curr instruction]
  (let [new-direction (update-direction (:direction curr) (first instruction))
        new-position (move (:position curr) new-direction (last instruction))]
    {:direction new-direction :position new-position}))

(defn part1 [input]
  (let [final-position (:position (reduce apply-instruction {:direction :north :position [0 0]} (parse-directions input)))]
    (+ (abs (first final-position)) (abs (last final-position)))))

(defn moves [position direction steps]
  (let [move (case direction
               :north [0 1]
               :east [1 0]
               :south [0 -1]
               :west [-1 0])]
    (rest (reductions #(mapv + %1 %2) position (repeat steps move)))))

(defn first-repeated [coll]
  (loop [seen #{}
         remaining coll]
    (let [current (first remaining)]
      (if (contains? seen current)
        current
        (recur (conj seen current) (rest remaining))))))

(defn apply-instruction-2 [curr instruction]
  (let [new-direction (update-direction (:direction curr) (first instruction))
        new-positions (moves (last (:positions curr)) new-direction (last instruction))
        positions (:positions curr)]
    {:direction new-direction :positions (into positions new-positions)}))

(defn part2 [input]
  (let [positions (reduce apply-instruction-2 {:direction :north :positions [[0 0]]} (parse-directions input))
        final-position (first-repeated (:positions positions))
        x (abs (first final-position))
        y (abs (last final-position))]
    (+ x y)))

(comment

  (def test-data "R5, L5, R5, R3")
  (part1 test-data)
  (part1 input)

  (def test-data-2 "R8, R4, R4, R8")
  (part2 test-data-2)
  (part2 input)

  )
