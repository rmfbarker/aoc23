(ns aoc-23.day01
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer :all]))

(defn decipher-calibration [s]
  (let [digits (re-seq #"\d" s)]
    (Integer/parseInt (str (first digits) (last digits)))))

(def alpha-nums {"one" 1 "two" 2 "three" 3 "four" 4 "five" 5 "six" 6 "seven" 7 "eight" 8 "nine" 9})

(defn sum-calibrations [input-strs]
  (reduce + (map decipher-calibration input-strs)))

(defn calculate-part1 []
  (sum-calibrations (str/split-lines (slurp (io/resource "input-day01.txt")))))

(deftest part1-checks
  (is (= 12 (decipher-calibration "1abc2")))
  (is (= 38 (decipher-calibration "pqr3stu8vwx")))
  (is (= 15 (decipher-calibration "a1b2c3d4e5f")))
  (is (= 77 (decipher-calibration "treb7uchet")))

  (is (= 142 (sum-calibrations (str/split-lines "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet")))))

(defn starts-with-num?
  "if the string starts with an alpha num, return the numeric number"
  [test-str]
  (reduce
    (fn [_ num]
      (if
        (str/starts-with? test-str num)
        (reduced (get alpha-nums num))))
    nil
    (keys alpha-nums)))

(defn transform-alphas [input-str]
  (loop [input-str input-str
         acc       ""]
    (if (seq input-str)
      (recur
        (subs input-str 1)                                  ;; drop the first char off the input
        (str acc (if-let [dig (starts-with-num? input-str)] ;; if we matched an alpha num then swap it in and move on
                   dig
                   (subs input-str 0 1))))
      acc)))

(defn calculate-part-2 []
  (reduce + (map (fn [line] (-> line transform-alphas decipher-calibration))
                 (str/split-lines (slurp (io/resource "input-day01.txt"))))))

