(ns advent-of-code.aoc2023.day01
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]))

(def input (line-seq (io/reader (io/resource "aoc2023/day01.txt"))))

(defn digits [s]
  (reduce (fn [acc c]
            (if (Character/isDigit c)
              (conj acc (str c))
              acc)) [] s))

(defn calibration-value [s]
  (let [d (digits s)
        fd (first d)
        ld (last d)]
    (parse-long (str/join [fd ld]))))

(defn part1 [input]
  (->> input
       (map calibration-value)
       (reduce +)))

(defn digits2 [input]
  (loop [digits []
         remaining input]
    (if (empty? remaining)
      digits
      (let [ch (first remaining)
            num-map {"one" "1" "two" "2" "three" "3" "four" "4" "five" "5" "six" "6" "seven" "7" "eight" "8" "nine" "9"}
            tail (subs remaining 1)]
        (cond
          (Character/isDigit ch) (recur (conj digits (str ch)) tail)
          :else (let [digit (some (fn [[k v]] (when (str/starts-with? remaining k) v)) num-map)]
                  (if digit
                    (recur (conj digits digit) tail)
                    (recur digits tail))))))))

(defn calibration-value2 [s]
  (let [d (digits2 s)
        fd (first d)
        ld (last d)]
    (parse-long (str/join [fd ld]))))

(defn part2 [input]
  (->> input
       (map calibration-value2)
       (reduce +)))

(comment
  (part1 input)

  (part2 input)

  )
