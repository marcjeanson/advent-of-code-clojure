(ns advent-of-code.aoc2016.day1
  (:require
   [clojure.java.io :as io]))


(def input (slurp (io/resource "aoc2016/day01.txt")))

(defn parse-directions [input]
  (let [matches (re-seq #"[RL]\d+" input)]
    (map (fn [match] [(first match) (Integer/parseInt (subs match 1))]) matches)))

(defn apply-instruction [curr instruction]
  (let [curr-direction (:direction curr)
        direction (first instruction)
        steps (last instruction)]
    (cond
      (and (= \R direction) (= curr-direction :n)) (-> curr (update :x + steps) (assoc :direction :e))
      (and (= \L direction) (= curr-direction :n)) (-> curr (update :x - steps) (assoc :direction :w))

      (and (= \R direction) (= curr-direction :s)) (-> curr (update :x - steps) (assoc :direction :w))
      (and (= \L direction) (= curr-direction :s)) (-> curr (update :x + steps) (assoc :direction :e))

      (and (= \R direction) (= curr-direction :e)) (-> curr (update :y - steps) (assoc :direction :s))
      (and (= \L direction) (= curr-direction :e)) (-> curr (update :y + steps) (assoc :direction :n))

      (and (= \R direction) (= curr-direction :w)) (-> curr (update :y + steps) (assoc :direction :n))
      (and (= \L direction) (= curr-direction :w)) (-> curr (update :y - steps) (assoc :direction :s)))))

(defn add-coordinate [[x y] [a b]]
  [(+ x a) (+ y b)])

(defn moves [start-point move steps]
  (rest (reductions add-coordinate start-point (repeat steps move))))

(defn apply-instruction-2 [curr instruction]
  (let [curr-direction (:direction curr)
        curr-position (last (:positions curr))
        direction (first instruction)
        steps (last instruction)]
    (cond
      (and (= \R direction) (= curr-direction :n)) (-> curr (update :positions into (moves curr-position [1 0] steps)) (assoc :direction :e))
      (and (= \L direction) (= curr-direction :n)) (-> curr (update :positions into (moves curr-position [-1 0] steps)) (assoc :direction :w))

      (and (= \R direction) (= curr-direction :s)) (-> curr (update :positions into (moves curr-position [-1 0] steps)) (assoc :direction :w))
      (and (= \L direction) (= curr-direction :s)) (-> curr (update :positions into (moves curr-position [1 0] steps)) (assoc :direction :e))

      (and (= \R direction) (= curr-direction :e)) (-> curr (update :positions into (moves curr-position [0 -1] steps)) (assoc :direction :s))
      (and (= \L direction) (= curr-direction :e)) (-> curr (update :positions into (moves curr-position [0 1] steps)) (assoc :direction :n))

      (and (= \R direction) (= curr-direction :w)) (-> curr (update :positions into (moves curr-position [0 1] steps)) (assoc :direction :n))
      (and (= \L direction) (= curr-direction :w)) (-> curr (update :positions into (moves curr-position [0 -1] steps)) (assoc :direction :s)))))

(defn part1 [input]
  (let [final-position (reduce apply-instruction {:direction :n :x 0 :y 0} (parse-directions input))]
    (+ (:x final-position) (:y final-position))))

(defn first-repeated [values]
  (loop [seen #{}
         remaining values]
    (let [current (first remaining)]
      (if (contains? seen current)
        current
        (recur (conj seen current) (rest remaining))))))


(defn part2 [input]
  (let [positions (reduce apply-instruction-2 {:direction :n :positions [[0 0]]} (parse-directions input))
        final-position (first-repeated (:positions positions))]
    (+ (abs (first final-position)) (abs (last final-position)))))

(comment
  (def test-data "R5, L5, R5, R3")
  (part1 input)

  (def test-data-2 "R8, R4, R4, R8")
  (part2 input)

  )
