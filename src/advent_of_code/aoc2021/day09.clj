(ns aoc2021.day09
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def input (line-seq (io/reader (io/resource "aoc2021/day09.txt"))))
(def dvec (->> input (mapv (fn [s] (mapv #(Character/digit % 10) s)))))

(def test-input ["2199943210"
                 "3987894921"
                 "9856789892"
                 "8767896789"
                 "9899965678"])

(defn convert-2d-vec [data]
  (->> data (mapv (fn [s] (mapv #(Character/digit % 10) s)))))

(comment

  ; part 1
  (let [hmap (convert-2d-vec input)
        height (count hmap)
        width (count (first hmap))]

    (apply + (for [x (range width)
                   y (range height)
                   :let [curr (get-in hmap [y x])
                         top (get-in hmap [(dec y) x])
                         bottom (get-in hmap [(inc y) x])
                         left (get-in hmap [y (dec x)])
                         right (get-in hmap [y (inc x)])]
                   :when (every? #(< curr %) (remove nil? [top bottom left right]))]
               (inc curr))))


  )
