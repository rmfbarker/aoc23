(ns aoc-23.day03
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]
            [clojure.string :as str]))

(defn get-digits [s]
  (re-seq #"\d+" s))

(def sample-file "sample-03.txt")
(def input-file "input-03")

(defn read-file [f] (str/split-lines (slurp (io/resource f))))

(def sample (read-file sample-file))

(defn create-index [s] (map
                         vector
                         (range)
                         s))

(def indexed-lines (create-index sample))

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

(defn check-row-before [indexed-lines row col digit])


(deftest part1-tests
  (is (= [0 5] (indexes-of
                 "346..346...*.....475.440....903&..996*...404+.395...*..............*.......&253.223.....................453..535......@....265.....290$........"
                 "346")))
  (is (= [0] (indexes-of
               "346.....*.....475.440....903&..996*...404+.395...*..............*.......&253.223.....................453..535......@....265.....290$........"
               "346")))
  (is (= [0 3] (indexes-of
                 "346346...*.....475.440....903&..996*...404+.395...*..............*.......&253.223.....................453..535......@....265.....290$........"
                 "346")))

  (let [indexed-file (create-index (read-file sample-file))
        [row line] (nth indexed-file 2)
        digits       (get-digits line)
        digit        (first digits)
        idxs         (indexes-of line digit)]
    ;; look at the row before this line and check if there is a symbol touching the number
    (check-row-before indexed-file row (first idxs) digit))


  (is (true? (check-row-before (read-file sample-file) 2 2 "35"))))

;; get the series of indexes that i need to check for a symbol.

;; for each line, get the list of numbers, get their indexes, look around those numbers for a symbol, keep numbers that are touching a symbol. sum those numbers that match that filter.
(defn sum-numbers [file-name]
  (let [indexed-file (create-index (read-file file-name))]
    (doseq [line indexed-file]
      (let [[row data] line
            digits (get-digits data)]
        (doseq [digit digits
                idx   (indexes-of data digit)]
          (println "row" row "col" idx "digit" digit))))))

(sum-numbers sample-file)


;; test lines with duplicate numbers
;(get-digits "346..346...*.....475.440....903&..996*...404+.395...*..............*.......&253.223.....................453..535......@....265.....290$........")
;(group-by identity (get-digits "346..346...*.....475.440....903&..996*...404+.395...*..............*.......&253.223.....................453..535......@....265.....290$........"))
;
;(parse-row [0 "346..346...*.....475.440....903&..996*...404+.395...*..............*.......&253.223.....................453..535......@....265.....290$........"])
