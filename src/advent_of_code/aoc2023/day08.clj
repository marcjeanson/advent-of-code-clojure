(ns advent-of-code.aoc2023.day08
  (:require
   [clojure.string :as str]
   [clojure.math.numeric-tower :refer [lcm]]
   [clojure.java.io :as io]))

(def input (line-seq (io/reader (io/resource "aoc2023/day08.txt"))))

(defn part1 [input]
  (let [instructions (cycle (first input))
        network (->> (nthrest input 2)
                     (reduce (fn [acc s]
                               (let [[node left right] (rest (re-find #"([A-Z][A-Z][A-Z]) = \(([A-Z][A-Z][A-Z]), ([A-Z][A-Z][A-Z])" s))]
                                 (assoc acc node {\L left \R right}))) {}))]
    (loop [steps 0
           curr "AAA"
           inst instructions]
      (if (= curr "ZZZ")
        steps
        (recur (inc steps) (get-in network [curr (first inst)]) (rest inst) )))))


(defn part2 [input]
  (let [instructions (cycle (first input))
        network (->> (nthrest input 2)
                     (reduce (fn [acc s]
                               (let [[node left right] (rest (re-find #"(\w\w\w) = \((\w\w\w), (\w\w\w)" s))]
                                 (assoc acc node {\L left \R right}))) {}))
        starting-nodes (->> (keys network)
                            (filter #(str/ends-with? % "A")))
        steps-for-node (fn [start]
                         (loop [steps 0
                                curr start
                                inst instructions]
                           (if (str/ends-with? curr "Z")
                             steps
                             (recur (inc steps) (get-in network [curr (first inst)]) (rest inst)))))]
    (->> starting-nodes
         (map #(steps-for-node %))
         (reduce lcm))))

(comment

  (def test-data ["LLR"
                  ""
                  "AAA = (BBB, BBB)"
                  "BBB = (AAA, ZZZ)"
                  "ZZZ = (ZZZ, ZZZ)"])

  (def test2-data ["LR"
                   ""
                   "11A = (11B, XXX)"
                   "11B = (XXX, 11Z)"
                   "11Z = (11B, XXX)"
                   "22A = (22B, XXX)"
                   "22B = (22C, 22C)"
                   "22C = (22Z, 22Z)"
                   "22Z = (22B, 22B)"
                   "XXX = (XXX, XXX)"])


  (part1 test-data) ;; => 6

  (part1 input) ;; => 16897

  (part2 test2-data) ;; => 6

  (part2 input) ;; => 16563603485021


(defn stackoverflow-part2 [input]
  (let [instructions (cycle (first input))
        network (->> (nthrest input 2)
                     (reduce (fn [acc s]
                               (let [[node left right] (rest (re-find #"(\w\w\w) = \((\w\w\w), (\w\w\w)" s))]
                                 (assoc acc node {\L left \R right}))) {}))
        starting-nodes (->> (keys network)
                            (filter #(str/ends-with? % "A")))]
    (loop [steps 0
           nodes starting-nodes
           inst instructions]
      (if (every? #(str/ends-with? % "Z") nodes)
        steps
        (let [next-inst (first inst)
              next-steps (map (fn [node]
                                (get-in network [node next-inst])) nodes)]
          (recur (inc steps) next-steps (rest inst)))))))

  )
