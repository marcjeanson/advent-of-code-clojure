(ns aoc2021.day03
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (line-seq (io/reader (io/resource "aoc2021/day03.txt"))))

(def test-data ["00100"
                 "11110"
                 "10110"
                 "10111"
                 "10101"
                 "01111"
                 "00111"
                 "11100"
                 "10000"
                 "11001"
                 "00010"
                 "01010"])

(defn most-common-bit [bits]
  (let [freqs (frequencies bits)
        count-1s (get freqs \1)
        count-0s (get freqs \0)]
    (if (>= count-1s count-0s)
      \1
      \0)))

(defn least-common-bit [bits]
  (let [freqs (frequencies bits)
        count-1s (get freqs \1)
        count-0s (get freqs \0)]
    (if (>= count-1s count-0s)
      \0
      \1)))

(defn binary-str-to-decimal [s]
  (read-string (str "2r" s)))

(defn filter-for-bit [bit nums fcn]
  (let [rotated (apply mapv str nums)
        bit-filter (fcn (get rotated bit))]
    (filterv #(= (get % bit) bit-filter) nums)))

(defn calc-rating [report fcn]
  (loop [bit 0
         nums-left report]
    (if (= 1 (count nums-left))
      (binary-str-to-decimal (first nums-left))
      (recur (inc bit) (filter-for-bit bit nums-left fcn)))))

(comment
  ; part 1
  (let [gamma (binary-str-to-decimal (apply str (map #(most-common-bit %) (apply map str input))))
        epsilon (binary-str-to-decimal (apply str (map #(least-common-bit %) (apply map str input))))]
    (* gamma epsilon))

  ; part 2
  (let [o (calc-rating input most-common-bit)
        c (calc-rating input least-common-bit)]
    (* o c))

  )
