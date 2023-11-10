(ns aoc2021.day08
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def input (line-seq (io/reader (io/resource "aoc2021/day08.txt"))))

(def test-input "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf")

(comment
  ; part 1
  ; lengths 2, 3, 4, 7
  (->> input
       (map #(-> %
                 (str/split #" \| ")
                 last
                 (str/split #" ")))
       (apply concat)
       (map #(count %))
       (filter #(or (= 2 %) (= 3 %) (= 4 %) (= 7 %)))
       count)



  (->> input
       (map #(-> %
                 (str/split #" \| ")
                 last
                 (str/split #" ")))
       (apply concat)
       distinct
       (filter (fn [s]
                 (let [len (count s)]
                   (or (= 2 len) (= 3 len) (= 4 len) (= 7 len)))))
       (sort-by #(count %)))

; 0: abcefg (6)
; 1: cf (2)
; 2: acdeg (5)
; 3: acdfg (5)
; 4: bcdf (4)
; 5: abdfg (5)
; 6: abdefg (6)
; 7: acf (3)
; 8: abcdefg (7)
; 9: abcdfg (6)

; unique values: 1(2), 4(4), 7(3), 8(7)

;;   0:      1:      2:      3:      4:
;;  aaaa    ....    aaaa    aaaa    ....
;; b    c  .    c  .    c  .    c  b    c
;; b    c  .    c  .    c  .    c  b    c
;;  ....    ....    dddd    dddd    dddd
;; e    f  .    f  e    .  .    f  .    f
;; e    f  .    f  e    .  .    f  .    f
;;  gggg    ....    gggg    gggg    ....

;;   5:      6:      7:      8:      9:
;;  aaaa    aaaa    aaaa    aaaa    aaaa
;; b    .  b    .  .    c  b    c  b    c
;; b    .  b    .  .    c  b    c  b    c
;;  dddd    dddd    ....    dddd    dddd
;; .    f  e    f  .    f  e    f  .    f
;; .    f  e    f  .    f  e    f  .    f
;;  gggg    gggg    ....    gggg    gggg
;;
; example:
; ab = 1
; dab = 7
; eafb = 4
; acedgfb = 8
;
; deduction:
; a = c, b = f, e = b, f = d

  (let [[input-str output-str] (str/split test-input #" \| ")
        signal-patterns (str/split input-str #" ")
        encoded-outputs (str/split output-str #" ")
        signals (set (flatten (conj signal-patterns encoded-outputs)))
        s1 (->> signals (drop-while #(not= 2 (count %))) first set)
        s7 (->> signals (drop-while #(not= 3 (count %))) first set)
        s4 (->> signals (drop-while #(not= 4 (count %))) first set)
        s8 (->> signals (drop-while #(not= 7 (count %))) first set)
        mapping {(first (set/difference s7 s1)) \a}
        ]
    mapping
    )
