(ns aoc-23.day03
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]
            [clojure.string :as str]))

(defn re-pos [re s]
  (loop [m   (re-matcher re s)
         res []]
    (if (.find m)
      (recur m (conj res [(.start m) (.group m)]))
      res)))

(defn get-digits
  "A distinct list of numbers appearing in a line, in order or occurrence"
  [s]
  (distinct (re-seq #"\d+" s)))

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
  (boolean
    (let [row-n (f-get-row row)]
      (if (< -1 row-n (count indexed-lines))
        (let [[_ line-of-interest] (nth indexed-lines row-n)
              start    (max 0 (dec col))
              end      (min (count line-of-interest)
                            (+ start 2 (count digit)))
              interest (subs line-of-interest start end)]

          (not (empty? (contains-symbol? interest))))))))

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
  [indexed-file line-y]
  (let [[row line] (nth indexed-file line-y)
        digits         (get-digits line)
        digit          (first digits)
        idxs           (indexes-of line digit)
        symbol-before? (touching-symbol-in-row-before? indexed-file row (first idxs) digit)
        symbol-row?    (touching-symbol-in-same-row? indexed-file row (first idxs) digit)
        symbol-after?  (touching-symbol-in-row-after? indexed-file row (first idxs) digit)]

    ;; look at the row before this line and check if there is a symbol touching the number

    [symbol-before? symbol-row? symbol-after?]
    ))

(defn check-around-specific-digit?
  [indexed-file line-y digit idx]
  (let [row            line-y
        symbol-before? (touching-symbol-in-row-before? indexed-file row idx digit)
        symbol-row?    (touching-symbol-in-same-row? indexed-file row idx digit)
        symbol-after?  (touching-symbol-in-row-after? indexed-file row idx digit)]

    ;; look at the row before this line and check if there is a symbol touching the number

    [symbol-before? symbol-row? symbol-after?]
    ))

(defn test-row [indexed-file row assertions]
  (let [[assert-previous assert-current assert-next] assertions
        [result-previous result-current result-next] (check-around? indexed-file row)]

    (is (= assert-previous result-previous))
    (is (= assert-current result-current))
    (is (= assert-next result-next))))

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

  (testing "symbols around digits"
    (let [indexed-file (create-index (read-file sample-file))]
      (test-row indexed-file 0 [false false true])
      (test-row indexed-file 2 [true false false])
      (test-row indexed-file 4 [false true false])
      (test-row indexed-file 6 [true false false])
      (test-row indexed-file 7 [false false true])
      ))
  )

;; get the series of indexes that i need to check for a symbol.

;; for each line, get the list of numbers, get their indexes, look around those numbers for a symbol, keep numbers that are touching a symbol. sum those numbers that match that filter.
(defn sum-numbers [file-name]
  (let [indexed-file (create-index (read-file file-name))]
    (doseq [line indexed-file]
      (let [[row data] line
            digits (re-pos #"\d+" data)
            ]
        (doseq [[idx digit] digits]
          ;(println "row" row "col" idx "digit" digit (check-around-specific-digit? indexed-file row digit idx))
          (if (some identity (check-around-specific-digit? indexed-file row digit idx))
            (println digit)
            ))))))

(defn -main [& args]
  ;(sum-numbers sample-file)
  (sum-numbers input-file)
  )


;; test lines with duplicate numbers
;(get-digits "346..346...*.....475.440....903&..996*...404+.395...*..............*.......&253.223.....................453..535......@....265.....290$........")
;(group-by identity (get-digits "346..346...*.....475.440....903&..996*...404+.395...*..............*.......&253.223.....................453..535......@....265.....290$........"))
;
;(parse-row [0 "346..346...*.....475.440....903&..996*...404+.395...*..............*.......&253.223.....................453..535......@....265.....290$........"])
