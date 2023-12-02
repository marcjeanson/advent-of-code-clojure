(ns advent-of-code.aoc2023.day02
  (:require
   [clojure.java.io :as io]))

(def input (line-seq (io/reader (io/resource "aoc2023/day02.txt"))))

(defn parse-possible-game [game-str]
  (let [[id-str turns-str] (rest (re-find #"Game (\d+): (.*)" game-str))
        id (parse-long id-str)
        valid-colour? (fn [pattern max-count]
                        (->> (re-seq pattern turns-str)
                             (mapv last)
                             (mapv parse-long)
                             (every? #(<= % max-count))))
        valid-red? (valid-colour? #"(\d+) red" 12)
        valid-green? (valid-colour? #"(\d+) green" 13)
        valid-blue? (valid-colour? #"(\d+) blue" 14)
        possible? (and valid-red? valid-green? valid-blue?)]
    {:id id :possible possible?}))

(defn parse-game-power [game-str]
  (let [turns-str (second (rest (re-find #"Game (\d+): (.*)" game-str)))
        max-colour (fn [pattern]
                     (->> (re-seq pattern turns-str)
                          (mapv last)
                          (mapv parse-long)
                          (apply max)))
        max-red (max-colour #"(\d+) red")
        max-green (max-colour #"(\d+) green")
        max-blue (max-colour #"(\d+) blue")]
    (* max-red max-green max-blue)))

(defn part1 [input]
  (->> input
       (map #(parse-possible-game %))
       (filter (fn [x] (true? (:possible x))))
       (reduce (fn [sum val]
                 (+ sum (:id val))) 0)))

(defn part2 [input]
  (->> input
       (map #(parse-game-power %))
       (reduce +)))

(comment
  (def test-data ["Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
                  "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
                  "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
                  "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
                  "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"])

  (part1 test-data) ;; => 8

  (part1 input) ;; => 2006

  (part2 test-data) ;; => 2286

  (part2 input) ;; => 84911

  )
