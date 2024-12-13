(ns advent-of-code.aoc2024.day13
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def input (line-seq (io/reader (io/resource "aoc2024/day13.txt"))))

(defn extract-nums [s]
  (->> s
       (re-seq #"\d+")
       (mapv parse-long)))

(defn extract-machines [input]
  (->> input
       (partition-by #(= "" %))
       (filter #(> (count %) 1))
       (mapv vec)
       (mapv (fn [machine]
               {:a (extract-nums (first machine))
                :b (extract-nums (second machine))
                :p (extract-nums (last machine))}))))

(defn cramer [{:keys [a b p]}]
  (let [ax (first a)
        ay (second a)
        bx (first b)
        by (second b)
        px (first p)
        py (second p)
        D   (- (* ax by) (* ay bx))
        D_a (- (* px by) (* py bx))
        D_b (- (* ax py) (* ay px))]
    (if (and (not= D 0)
             (zero? (mod D_a D))
             (zero? (mod D_b D)))
      [(quot D_a D) (quot D_b D)]
      [])))

(defn part1 [input]
  (->>
   (extract-machines input)
   (mapv cramer)
   (filter seq)
   (reduce (fn [tokens [a b]]
             (+ tokens
                (+ b (* 3 a)))) 0)))

(defn bump-p [{:keys [p] :as machine}]
  (let [new-p (mapv #(+ 10000000000000 %) p)]
    (assoc machine :p new-p)))


(defn part2 [input]
  (->>
   (extract-machines input)
   (mapv bump-p)
   (mapv cramer)
   (filter seq)
   (reduce (fn [tokens [a b]]
             (+ tokens
                (+ b (* 3 a)))) 0)))

(comment

  (def test-input '(
                     "Button A: X+94, Y+34"
                     "Button B: X+22, Y+67"
                     "Prize: X=8400, Y=5400"
                     ""
                     "Button A: X+26, Y+66"
                     "Button B: X+67, Y+21"
                     "Prize: X=12748, Y=12176"
                     ""
                     "Button A: X+17, Y+86"
                     "Button B: X+84, Y+37"
                     "Prize: X=7870, Y=6450"
                     ""
                     "Button A: X+69, Y+23"
                     "Button B: X+27, Y+71"
                     "Prize: X=18641, Y=10279"))

  (part1 test-input) ;; => 480
  (part1 input) ;; => 26005

  (part2 test-input) ;; => 875318608908
  (part2 input) ;; => 105620095782547

  )
