(ns aoc2021.day02
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (line-seq (io/reader (io/resource "aoc2021/day02.txt"))))

(def test-data '("forward 5"
                 "down 5"
                 "forward 8"
                 "up 3"
                 "down 8"
                 "forward 2"))

(defn parse-command [str]
  (let [[command n] (str/split str #" ")
        value (Integer/parseInt n)]
    (case command
      "forward" [value 0]
      "down" [0 value]
      "up" [0 (* -1 value)])))

(defn parse-command-2 [str aim]
  (let [[command n] (str/split str #" ")
        value (Integer/parseInt n)]
    (case command
      "forward" [value (* aim value) 0]
      "down" [0 0 value]
      "up" [0 0 (* -1 value)])))

(comment
  (parse-command "down 5")

  ; part 1
  (reduce (fn [acc command]
            (map + acc (parse-command command))) [0 0] test-data)

  (apply * (reduce (fn [acc command]
                     (map + acc (parse-command command))) [0 0] input))

  ; part 2
  (let [[h d a] (reduce (fn [acc command]
                          (map + acc (parse-command-2 command (last acc)))) [0 0 0] test-data)]
    (* h d))

  (let [[h d a] (reduce (fn [acc command]
                          (map + acc (parse-command-2 command (last acc)))) [0 0 0] input)]
    (* h d))

  )
