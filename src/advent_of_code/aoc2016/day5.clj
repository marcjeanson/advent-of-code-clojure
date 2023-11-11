(ns advent-of-code.aoc2016.day5
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [md5.core :as md5]))

(def input "uqwqemis")

(defn matching-md5 [hex]
  (->> (subs hex 0 5)
       (every? #(= \0 %))))

(defn part-1 [input]
  (->> (range)
       (map #(-> (str input %)
                 md5/string->md5-hex
                 (subs 0 6)))
       (filter #(matching-md5 %))
       (take 8)
       (map #(nth % 5))
       (apply str)))

(defn matching-md5-2 [hex]
  (re-find #"^00000[0-7]." hex))

(defn update-password [password [position ch]]
    (let [int-pos (Character/digit position 10)]
      (if (get password int-pos)
        password
        (assoc password int-pos ch))))

(defn part-2 [input]
  (->> (range)
       (map #(-> (str input %)
                 md5/string->md5-hex
                 (subs 0 7)))
       (filter #(matching-md5-2 %))
       (map #(vector (nth % 5) (nth % 6)))
       (reduce (fn [password pair]
                 (if (every? some? password)
                   (reduced password)
                   (update-password password pair))) (vec (repeat 8 nil)))
       (apply str)))

(comment

  )
