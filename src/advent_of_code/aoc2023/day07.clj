(ns advent-of-code.aoc2023.day07
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]))

(def input (line-seq (io/reader (io/resource "aoc2023/day07.txt"))))

(def card-values { \A 14 \K 13 \Q 12 \J 11 \T 10 \9 9 \8 8 \7 7 \6 6 \5 5 \4 4 \3 3 \2 2 })
(def joker-card-values { \A 14 \K 13 \Q 12 \T 10 \9 9 \8 8 \7 7 \6 6 \5 5 \4 4 \3 3 \2 2 \J 1 })

(defn hand-type [freq]
  ;; return a 1-7 value for hand-type. 7 being a 5 of a kind
  (let [max-freq (->> freq vals (apply max))
        cnt-freq (count freq)]
    (cond
      (= cnt-freq 1) 7
      (= max-freq 4) 6
      (= cnt-freq 2) 5
      (= max-freq 3) 4
      (= cnt-freq 3) 3
      (= cnt-freq 4) 2
      (= cnt-freq 5) 1)))

(defn card-compare [[x y]]
  (let [x-val (card-values x)
        y-val (card-values y)]
    (compare x-val y-val)))

(defn hand-compare [x y]
  ;; -1 if x comes before y, 1 if x comes after y, 0 if they are equal
  (let [x-val (hand-type (frequencies x))
        y-val (hand-type (frequencies y))
        xy-compare (compare x-val y-val)]
    (if (zero? xy-compare)
      (->> (map vector x y)
           (map card-compare)
           (filter #(not= 0 %))
           first)
      xy-compare)))

(defn part1 [input]
  (->> input
       (map (fn [row]
              (let [[hand bid] (str/split row #" ")]
                {:hand hand :bid (parse-long bid)})))
       (sort-by :hand hand-compare)
       (map-indexed (fn [idx hand]
                      (* (inc idx) (:bid hand))))
       (reduce +)))

(defn joker-hand-type [freq]
 (let [has-joker? (contains? freq \J)
       only-jokers? (= 5 (get freq \J))]
   (if (and has-joker? (not only-jokers?))
     (let [sorted-freqs (into {} (sort-by last #(> %1 %2) freq))
           num-jokers (get sorted-freqs \J)
           card-to-pretend (some #(when (not= \J (key %)) (key %)) sorted-freqs)
           pretend-freqs (-> sorted-freqs
                              (update card-to-pretend + num-jokers)
                              (dissoc \J))]
       (hand-type pretend-freqs))
     (hand-type freq))))


(defn joker-card-compare [[x y]]
  (let [x-val (joker-card-values x)
        y-val (joker-card-values y)]
    (compare x-val y-val)))

(defn joker-hand-compare [x y]
  ;; -1 if x comes before y, 1 if x comes after y, 0 if they are equal
  (let [x-val (joker-hand-type (frequencies x))
        y-val (joker-hand-type (frequencies y))
        xy-compare (compare x-val y-val)]
    (if (zero? xy-compare)
      (->> (map vector x y)
           (map joker-card-compare)
           (filter #(not= 0 %))
           first)
      xy-compare)))

(defn part2 [input]
  (->> input
       (map (fn [row]
              (let [[hand bid] (str/split row #" ")]
                {:hand hand :bid (parse-long bid)})))
       (sort-by :hand joker-hand-compare)
       (map-indexed (fn [idx hand]
                      (* (inc idx) (:bid hand))))
       (reduce +)))

(comment

  (def test-data ["32T3K 765"
                  "T55J5 684"
                  "KK677 28"
                  "KTJJT 220"
                  "QQQJA 483"])


  (part1 test-data) ;; => 6440

  (part1 input) ;; => 248113761

  (part2 test-data) ;; => 5905

  (part2 input) ;; => 246285222

  )
