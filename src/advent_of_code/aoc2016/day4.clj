(ns advent-of-code.aoc2016.day4
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def input (line-seq (io/reader (io/resource "aoc2016/day04.txt"))))

(defn sort-map-desc [m]
  (sort-by (juxt (comp - val) key) m))

(defn calc-checksum [name]
    (let [name (str/replace name #"\-" "")]
      (->> name
          frequencies
          sort-map-desc
          (take 5)
          (map first)
          (apply str))))

(defn valid-rooms [input]
  (->> input
       (map #(rest (re-find #"(^[a-z-]+)\-(\d+)\[([a-z]+)" %)))
       (filter (fn [[room _sector checksum]]
                 (= (calc-checksum room) checksum)))))

(defn part-1 [input]
  (->> (valid-rooms input)
       (map fnext)
       (map #(Integer/parseInt %))
       (reduce +)))

(defn decrypt-char [c shift]
  (if (= c \-)
    \space
    (-> c
        (int)
        (- (int \a))
        (+ shift)
        (mod 26)
        (+ (int \a))
        (char))))

(defn decrypt [cipertext shift]
  (let [decrypted (->> cipertext
                       (map #(decrypt-char % (Integer/parseInt shift)))
                       (apply str))]
    {:text decrypted :sector shift}))

(defn part-2 [input]
  (->> (valid-rooms input)
       (map #(decrypt (first %) (fnext %)))
       (filter #(re-find #"north" (:text %)))
       first
       :sector))

(comment
  (def sample '("aaaaa-bbb-z-y-x-123[abxyz]"
                "a-b-c-d-e-f-g-h-987[abcde]"
                "not-a-real-room-404[oarel]"
                "totally-real-room-200[decoy]"))

  (def room-name "aaaaa-bbb-z-y-x")

  (part-1 sample)
  (part-1 input)

  (part-2 input)

  )
