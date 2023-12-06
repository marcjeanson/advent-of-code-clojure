(ns advent-of-code.aoc2023.day06)

(defn ways-to-win [data]
    (->> data
         (map (fn [[t d]]
                (->> (range 1 t)
                     (mapv (fn [x]
                             (* x (- t x))))
                     (filter #(> % d))
                     (count))))
         (reduce *)))

(comment
  ;; Part 1 Test data
  ;; Time:      7  15   30
  ;; Distance:  9  40  200

  (ways-to-win [[7 9] [15 40] [30 200]]) ;; => 288

  ;; Part 1 Puzzle input
  ;; Time:        42     89     91     89
  ;; Distance:   308   1170   1291   1467

  (ways-to-win [[42 308] [89 1170] [91 1291] [89 1467]]) ;; => 3317888

  ;; Part 2 Test data
  ;; Time:      71530
  ;; Distance:  940200

  (ways-to-win [[71530 940200]]) ;; => 71503

  ;; Part 2 Puzzle input
  ;; Time:        42899189
  ;; Distance:   308117012911467

  (ways-to-win [[42899189 308117012911467]]) ;; => 24655068

  )
