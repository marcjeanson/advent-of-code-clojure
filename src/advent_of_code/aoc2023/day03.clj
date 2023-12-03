(ns advent-of-code.aoc2023.day03
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]))

(def input (line-seq (io/reader (io/resource "aoc2023/day03.txt"))))

(defn is-symbol? [ch]
  (not (or (Character/isDigit ch) (= \. ch))))

(defn find-symbols [grid]
  (for [x (range (count grid))
        y (range (count (first grid)))
        :let [xy (get-in grid [x y])]
        :when (is-symbol? xy)]
    [x y]))

(defn is-digit? [grid [x y]]
  (let [ch (get-in grid [x y])]
    (if (nil? ch)
      false
      (Character/isDigit ch))))

(defn adjacent-digit-coords [grid [x y]]
  (let [adj-coords [[(dec x) (dec y)]
                    [(dec x) y]
                    [(dec x) (inc y)]
                    [x (dec y)]
                    [x (inc y)]
                    [(inc x) (dec y)]
                    [(inc x) y]
                    [(inc x) (inc y)]]]
    (filterv (fn [coord]
               (is-digit? grid coord)) adj-coords)))

(defn part-number-coord [grid [x y]]
  (let [row-coords (for [idx (reverse (range (inc y)))]
                     [x idx])]
    (last (take-while #(is-digit? grid %) row-coords))))

(defn part-number [grid [x y]]
  (let [row-str (subs (get grid x) y)]
    (re-find #"\d+" row-str)))

(defn part1 [input]
  (let [grid (vec input)]
    (->> grid
         (find-symbols)
         (map #(adjacent-digit-coords grid %))
         (flatten)
         (partition 2)
         (map #(part-number-coord grid %))
         (distinct)
         (map #(part-number grid %))
         (map parse-long)
         (reduce +))))

(defn find-stars [grid]
  (for [x (range (count grid))
        y (range (count (first grid)))
        :let [xy (get-in grid [x y])]
        :when (= \* xy)]
    [x y]))

(defn valid-gear? [grid [x y]]
  (let [left-count (if (is-digit? grid [x (dec y)]) 1 0)
        right-count (if (is-digit? grid [x (inc y)]) 1 0)
        top-str (str (get-in grid [(dec x) (dec y)])
                     (get-in grid [(dec x) y])
                     (get-in grid [(dec x) (inc y)]))
        bottom-str (str (get-in grid [(inc x) (dec y)])
                     (get-in grid [(inc x) y])
                     (get-in grid [(inc x) (inc y)]))
        top-count (count (remove #(= "" %) (str/split top-str #"\D")))
        bottom-count (count (remove #(= "" %) (str/split bottom-str #"\D")))]
    (= (+ left-count right-count top-count bottom-count) 2)))

(defn part2 [input]
  (let [grid (vec input)]
    (->> grid
         (find-stars)
         (filterv #(valid-gear? grid %))
         (mapv #(adjacent-digit-coords grid %))
         (mapv (fn [coords]
                (mapv #(part-number-coord grid %) coords)))
         (mapv (fn [coords]
                (mapv #(part-number grid %) coords)))
         (mapv distinct)
         (mapv (fn [coords]
                 (let [first (parse-long (first coords))
                       second (if (= 1 (count coords)) first (parse-long (second coords)))]
                   (* first second))))
         (reduce +))))

(comment

  (def test-data '("467..114.."
                   "...*......"
                   "..35..633."
                   "......#..."
                   "617*......"
                   ".....+.58."
                   "..592....."
                   "......755."
                   "...$.*...."
                   ".664.598.."))

  (part1 test-data) ;; => 4361

  (part1 input) ;; => 535235

  (part2 test-data) ;; => 467835

  (part2 input) ;; => 79844424

  )
