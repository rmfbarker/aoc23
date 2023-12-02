(ns aoc-23.day02
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer :all]))

(def sample-file "sample-day02.txt")
(def input-file "input-day02.txt")

(defn read-file [f] (str/split-lines (slurp (io/resource f))))

"12 red cubes, 13 green cubes, and 14 blue cubes"
(def cube-limits {"red" 12 "green" 13 "blue" 14})

(defn parse-game [game-line] (re-seq
                               #"\d+ blue|\d+ red|\d+ green"
                               game-line))

(def test-games (map parse-game (read-file sample-file)))

(defn game-ok? [test-game]
  (reduce
    (fn [_ sample]
      (let [[cubes colour] (str/split sample #" ")
            limit (get cube-limits colour)
            exceeded (< limit (Integer/parseInt cubes))]
          (if exceeded
            (reduced false)
            true)))
    nil
    test-game))

(defn calculate-id-total [input-file]
  (let [results (map game-ok? (map parse-game (read-file input-file)))]
    (loop [results results
           total 0
           ids (iterate inc 1)]

      (if (seq results) (recur (rest results) (+ total
                                                 (if (first results) (first ids) 0))
                               (rest ids))
                        total))))

(calculate-id-total sample-file)
(calculate-id-total input-file)
