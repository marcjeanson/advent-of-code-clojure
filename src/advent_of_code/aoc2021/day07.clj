(ns aoc2021.day07
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (slurp (io/resource "aoc2021/day07.txt")))

(def test-input "16,1,2,0,4,2,7,1,2,14")

(comment
  ; part 1
  ; guess with calc median value
  (->
   (mapv #(Integer/parseInt %) (str/split (str/trim input) #","))
   sort
   vec
   (get 500)
   )

  (apply + (map #(Math/abs (- 314 %)) (mapv #(Integer/parseInt %) (str/split (str/trim input) #","))))

  ; part 2
  ; guess by using the average
  (Math/round (/ (reduce + (mapv #(Integer/parseInt %) (str/split (str/trim input) #","))) 1000.0))
; 447 is average but didn't work. Rounded down to 446 which works

  (apply + (map (fn [x]
                  (let [diff (Math/abs (- 446 x))]
                    (quot (* diff (inc diff)) 2))) (mapv #(Integer/parseInt %) (str/split (str/trim input) #","))))

; 87640262 (with 447)
; 87640209 (with 446
  )
