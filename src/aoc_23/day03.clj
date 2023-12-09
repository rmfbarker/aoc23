(ns aoc-23.day03
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]
            [clojure.string :as str]))

(defn get-digits [s]
  (re-seq #"\d+" s))

(def sample-file "sample-03.txt")
(def input-file "input-03.txt")

(defn read-file [f] (str/split-lines (slurp (io/resource f))))

(def sample (read-file sample-file))

(def indexed-lines (map
                     vector
                     (range)
                     sample))

;; The below does not handle lines with repeated numbers because it uses 'substring'
(defn parse-row [row-line]
  (let [[row line] row-line
        numbers (get-digits line)]
    (println numbers)
    (reduce
      (fn [acc n]
        (let [idx (str/index-of line n)]
          (conj acc [n row
                     idx (+ idx (count n))])))
      []
      numbers)))

(parse-row (first
             indexed-lines))


;; co-ords for all numbers in the schematic
(apply concat (map parse-row indexed-lines))





(defn indexes-of
  "Finds the starting index for all occurrences of a given substring"
  [line of]
  (loop [pointer 0
         acc     []]
    (if-let [idx (str/index-of line of pointer)]
      (recur (+ 1 idx pointer) (conj acc idx))
      acc)))


(deftest part1-tests
  (is (= [0 5] (indexes-of
                 "346..346...*.....475.440....903&..996*...404+.395...*..............*.......&253.223.....................453..535......@....265.....290$........"
                 "346")))
  (is (= [0] (indexes-of
                 "346.....*.....475.440....903&..996*...404+.395...*..............*.......&253.223.....................453..535......@....265.....290$........"
                 "346")))
  (is (= [0 3] (indexes-of
                 "346346...*.....475.440....903&..996*...404+.395...*..............*.......&253.223.....................453..535......@....265.....290$........"
                 "346"))))

  ;; get the series of indexes that i need to check for a symbol.







;; test lines with duplicate numbers
;(get-digits "346..346...*.....475.440....903&..996*...404+.395...*..............*.......&253.223.....................453..535......@....265.....290$........")
;(group-by identity (get-digits "346..346...*.....475.440....903&..996*...404+.395...*..............*.......&253.223.....................453..535......@....265.....290$........"))
;
;(parse-row [0 "346..346...*.....475.440....903&..996*...404+.395...*..............*.......&253.223.....................453..535......@....265.....290$........"])
