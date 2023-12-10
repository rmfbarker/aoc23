(ns aoc-23.day03
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]
            [clojure.string :as str]))

(defn re-pos [re s]
  (loop [m   (re-matcher re s)
         res {}]
    (if (.find m)
      (recur m (assoc res [(.start m) (.end m)] (.group m)))
      res)))

(defn digit-index [data] (re-pos #"\d+" data))

(defn asterisk-indexes [data] (keys (re-pos #"\*" data)))

(defn contains-symbol? [s]
  (re-seq #"[^a-zA-Z\d\s\.]" s))

(def sample-file "sample-03.txt")
(def input-file "input-03")

(defn read-file [f] (str/split-lines (slurp (io/resource f))))

(defn create-index [s] (map
                         vector
                         (range)
                         s))

;; The below does not handle lines with repeated numbers because it uses 'substring'
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

(defn check-around-specific-digit?
  [indexed-file line-y digit idx]
  (let [row            line-y
        symbol-before? (touching-symbol-in-row-before? indexed-file row idx digit)
        symbol-row?    (touching-symbol-in-same-row? indexed-file row idx digit)
        symbol-after?  (touching-symbol-in-row-after? indexed-file row idx digit)]

    ;; look at the row before this line and check if there is a symbol touching the number

    [symbol-before? symbol-row? symbol-after?]
    ))

(defn test-row
  "Used for testing purposes only.  Convenience method." [indexed-file row digit-n assertions]
  (let [[assert-previous assert-current assert-next] assertions
        line      (nth indexed-file row)
        [row data] line
        digit-idx (digit-index data)
        [[idx] dig] (nth (seq digit-idx) digit-n)
        [result-previous result-current result-next] (check-around-specific-digit? indexed-file row dig idx)]

    (is (= assert-previous result-previous))
    (is (= assert-current result-current))
    (is (= assert-next result-next))))

(defn sum-line [indexed-file row-n]
  (let [line (nth indexed-file row-n)
        [row data] line]
    (reduce (fn [acc [[idx _] dig]]
              (if (some true? (check-around-specific-digit? indexed-file row dig idx))
                (+ acc (Integer/parseInt dig))
                acc))
            0
            (digit-index data))))


(defn sum-numbers [file-name]
  (let [indexed-file (create-index (read-file file-name))]
    (reduce (fn [acc row-n]
              (+ acc (sum-line indexed-file row-n)))
            0
            (range (count indexed-file)))))

;; for each line, get the list of numbers, get their indexes, look around those numbers for a symbol, keep numbers that are touching a symbol. sum those numbers that match that filter.
(deftest part1-tests
  (testing "symbols around digits"
    (let [indexed-file (create-index (read-file sample-file))]
      (test-row indexed-file 0 0 [false false true])
      (test-row indexed-file 0 1 [false false false])
      (test-row indexed-file 2 0 [true false false])
      (test-row indexed-file 4 0 [false true false])
      (test-row indexed-file 6 0 [true false false])
      (test-row indexed-file 7 0 [false false true])
      ))

  (testing "overall result"
    (is (= 498559 (sum-numbers input-file))))

  )

(defn -main [& args]
  ;(sum-numbers sample-file)
  (println (sum-numbers input-file))
  )


;; test lines with duplicate numbers
;(get-digits "346..346...*.....475.440....903&..996*...404+.395...*..............*.......&253.223.....................453..535......@....265.....290$........")
;(group-by identity (get-digits "346..346...*.....475.440....903&..996*...404+.395...*..............*.......&253.223.....................453..535......@....265.....290$........"))
;
;(parse-row [0 "346..346...*.....475.440....903&..996*...404+.395...*..............*.......&253.223.....................453..535......@....265.....290$........"])

(defn overlaps [])

(defn all-digits-surrounding-lines [lines row-y]
  (let [digits-before (map vec (digit-index (nth lines (dec row-y))))
        digits-same   (map vec (digit-index (nth lines row-y)))
        digits-after  (map vec (digit-index (nth lines (inc row-y))))]
    (concat digits-after digits-same digits-before)))

(defn adjacent-numbers [lines row-y col-x]
  (map second
       (filter (fn [[[start end] digit]]
                 (<= start (inc col-x) (inc end)))
               (all-digits-surrounding-lines lines row-y)
               ))
  )

(defn parse-int [i] (Integer/parseInt i))

(defn adjacent-numbers-summed [lines row-y col-x]
  (reduce * (map parse-int (adjacent-numbers lines row-y col-x)))
  )


(defn gear-ratio-sum-line [lines row-y]
  (reduce +
          (map
            (fn [ast] (adjacent-numbers-summed lines row-y (first ast)))

            (filter
              (fn [x]
                (= 2 (count (adjacent-numbers lines row-y (first x)))))
              (asterisk-indexes (nth lines row-y)))))
  )

(defn gear-ratio-sum
  "Takes a line, finds all asterisks and returns the sum of the gear ratios on that line"
  [lines]
  (reduce + (map (partial gear-ratio-sum-line lines) (range (count lines)))))

(deftest part2-tests


  (testing "part two"

    (let [sample-input (read-file sample-file)          ]

      (testing "asterisk indexes"

        (is (= [[5 6]] (asterisk-indexes (nth sample-input 8))))    ; => [[5 6]]

        (is (= 3 (first (first (asterisk-indexes (nth sample-input 1)))))) ; => 3
        (is (= 3 (first (first (asterisk-indexes (nth sample-input 4)))))) ; => 3
        (is (= 5 (first (first (asterisk-indexes (nth sample-input 8)))))) ; => 5
        )

      (is (= (set (adjacent-numbers sample-input 1 3))
             #{"467" "35"}))

      (is (= (set (adjacent-numbers sample-input 4 3))
             #{"617"}))

      (is (= (set (adjacent-numbers sample-input 8 5))
             #{"755" "598"}))

      (is (= (adjacent-numbers-summed sample-input 1 3)
             16345))

      (is (= (adjacent-numbers-summed sample-input 8 5)
             451490))

      (is (= (gear-ratio-sum-line sample-input 1)
             16345)
          "Calculate the sum of the gear ratios for line 1")

      (is (= 467835 (gear-ratio-sum sample-input))
          "Calculate the gear ratio sum for the input sample, for all lines")

      (is (= 72246648 (gear-ratio-sum (read-file input-file)))
          "Calculate the answer for day 3 part 2; the gear ratio sum for the input, for all lines")

      )))
