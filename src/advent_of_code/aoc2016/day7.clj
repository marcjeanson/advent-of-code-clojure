(ns advent-of-code.aoc2016.day7
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def input (line-seq (io/reader (io/resource "aoc2016/day07.txt"))))

(defn extract-hypernets [s]
  (map second (re-seq #"\[([^\]]+)\]" s)))

(defn extract-supernets [s]
  (clojure.string/split (clojure.string/replace s #"\[[^\]]+\]" " ") #"\s+"))

(defn is-abba? [s]
  (and (not= (first s) (second s)) (= (first s) (last s)) (= (second s) (nth s 2))))

(defn has-abba? [s]
  (some #(is-abba? %) (partition 4 1 s)))

(defn part-1 [input]
  (->> input
       (map (fn [s]
              {:hypernets (extract-hypernets s) :supernets (extract-supernets s)}))
       (map (fn [ip]
              (and (not-any? has-abba? (:hypernets ip)) (some has-abba? (:supernets ip)))))
       (filter true?)
       count))

(defn is-aba? [s]
  (and (= (first s) (last s)) (not= (first s) (second s))))

(defn extract-abas [hypernets]
  (->> hypernets
       (reduce (fn [acc hypernet]
                 (->> (partition 3 1 hypernet)
                      (filter is-aba?)
                      (map #(apply str %))
                      (into acc))) [])))

(defn convert-abas-to-babs [aba]
  (str (second aba) (first aba) (second aba)))

(defn extract-babs [hypernets]
  (->> (extract-abas hypernets)
       (map convert-abas-to-babs)))

(defn part-2 [input]
  (->> input
       (map (fn [s]
              {:hypernets (extract-hypernets s) :supernets (extract-supernets s)}))
       (map (fn [ip]
              {:abas (extract-abas (:supernets ip)) :babs (extract-babs (:hypernets ip))}))
       (map (fn [ip]
              (some (set (:abas ip)) (:babs ip))))
       (filter identity)
       count))

(comment
  (def sample "gdlrknrmexvaypu[crqappbbcaplkkzb]vhvkjyadjsryysvj[nbvypeadikilcwg]jwxlimrgakadpxu[dgoanojvdvwfabtt]yqsalmulblolkgsheo")

  (def test ["abba[mnop]qrst"
             "abcd[bddb]xyyx"
             "aaaa[qwer]tyui"
             "ioxxoj[asdfgh]zxcvbn"])

  (part-1 test)
  (part-1 input)

  (def test-2 ["aba[bab]xyz"
               "xyx[xyx]xyx"
               "aaa[kek]eke"
               "zazbz[bzb]cdb"])

  (part-2 test-2)
  (part-2 input)

)
