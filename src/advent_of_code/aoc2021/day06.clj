(ns aoc2021.day06
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (slurp (io/resource "aoc2021/day06.txt")))


;; slow way
(def test-input "3,4,3,1,2")

(defn tick [state]
  (let [zero-count (count (filter zero? state))
        add-9s (flatten (-> state
                            (conj (take zero-count (repeat 9)))))]
    (mapv (fn [x]
           (if (zero? x)
             6
             (dec x))) add-9s)))


(defn fast-solve [days input]
  (reduce (fn [generation _]
            (reduce (fn [new-gen [tick conut]]
                      (if (> tick 0)
                             (update new-gen (dec tick) (fnil + 0) conut)
                             (-> new-gen
                                 (update 6 (fnil + 0) conut)
                                 (update 8 (fnil + 0) conut))))
                      {} generation))
            (frequencies input)
            (range days)))

(comment
  (->> (take 81 (iterate tick (mapv #(Integer/parseInt %) (str/split (str/trim input) #","))))
       last
       count)

  (reduce + (vals (fast-solve 80 [3 4 3 1 2])))
  (reduce + (vals (fast-solve 256 (mapv #(Integer/parseInt %) (str/split (str/trim input) #",")))))

  )



