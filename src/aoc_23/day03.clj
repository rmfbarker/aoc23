(ns aoc-23.day03
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]
            [clojure.string :as str]))

(defn get-digits [s]
  (re-seq #"\d+" s))

(defn contains-symbol? [s]
  (re-seq #"[^a-zA-Z\d\s\.]" s))

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

(defn touching-symbol-in-row-x? [f-get-row indexed-lines row col digit]
  (let [row-n (f-get-row row)]
    (println "about to check row-n" row-n "row count is" (count indexed-lines))
    (if (< -1 row-n (count indexed-lines))
      (let [[_ line-of-interest] (nth indexed-lines row-n)
            start    (max 0 (dec col))
            end      (min (inc (count line-of-interest))
                          (+ start 1 (count digit)))
            interest (subs line-of-interest start end)]
        (println "line of interest" line-of-interest "digit length" (count digit) "piece of interest" interest)

        (not (empty? (contains-symbol? interest)))))))

(def touching-symbol-in-same-row?
  (partial touching-symbol-in-row-x? identity))

(def touching-symbol-in-row-before?
  (partial touching-symbol-in-row-x? dec))

(def touching-symbol-in-row-after?
  (partial touching-symbol-in-row-x? inc))


(defn check-around?
  "This only checks the first digit in the line and the first occurrence

  Needs to be parameterised to take a digit and the index of that digit

  And called by another function that is processing the digits in a row
  "
  [file line-y]
  (let [indexed-file   (create-index (read-file file))
        [row line] (nth indexed-file line-y)
        digits         (get-digits line)
        digit          (first digits)
        idxs           (indexes-of line digit)
        symbol-before? (touching-symbol-in-row-before? indexed-file row (first idxs) digit)
        symbol-row?    (touching-symbol-in-same-row? indexed-file row (first idxs) digit)
        symbol-after?  (touching-symbol-in-row-after? indexed-file row (first idxs) digit)]

    (println line digits idxs symbol-before? symbol-row? symbol-after?)
    ;; look at the row before this line and check if there is a symbol touching the number

    [symbol-before? symbol-row? symbol-after?]
    ))



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

  (let [row 0
        [symbol-prior-row
         symbol-in-row
         symbol-subs-row] (check-around? sample-file row)]
    (is ((complement true?) symbol-prior-row))
    (is (false? symbol-in-row))
    (is (true? symbol-subs-row)))

  (let [[touches-symbol
         touches-before
         touches-symbol-after] (check-around? sample-file 2)]
    (is (true? touches-symbol))
    (is (false? touches-before))
    (is (false? touches-symbol-after)))

  (let [[touches-symbol
         touches-symbol-row
         touches-symbol-after] (check-around? sample-file 7)]
    (is (false? touches-symbol))
    (is (false? touches-symbol-row))
    (is (true? touches-symbol-after)))

  (let [[symbol-before?
         symbol-row?
         symbol-after?] (check-around? sample-file 4)]
    (is (false? symbol-before?))
    (is (true? symbol-row?))
    (is (false? symbol-after?))))

;; get the series of indexes that i need to check for a symbol.

;; for each line, get the list of numbers, get their indexes, look around those numbers for a symbol, keep numbers that are touching a symbol. sum those numbers that match that filter.
(defn sum-numbers [file-name]
  (let [indexed-file (create-index (read-file file-name))]
    (doseq [line indexed-file]
      (let [[row data] line
            digits (get-digits data)]
        (doseq [digit digits
                idx   (indexes-of data digit)]
          (println "row" row "col" idx "digit" digit (check-around? file-name row)))))))

(defn -main [& args]
  (sum-numbers sample-file))


;; test lines with duplicate numbers
;(get-digits "346..346...*.....475.440....903&..996*...404+.395...*..............*.......&253.223.....................453..535......@....265.....290$........")
;(group-by identity (get-digits "346..346...*.....475.440....903&..996*...404+.395...*..............*.......&253.223.....................453..535......@....265.....290$........"))
;
;(parse-row [0 "346..346...*.....475.440....903&..996*...404+.395...*..............*.......&253.223.....................453..535......@....265.....290$........"])
