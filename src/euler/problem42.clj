(ns euler.problem42
  (:require [clojure.string :as str]))

;; The nth term of the sequence of triangle numbers is given by, tn =
;; ½n (n+1); so the first ten triangle numbers are:

;; 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

;; By converting each letter in a word to a number corresponding to
;; its alphabetical position and adding these values we form a word
;; value. For example, the word value for SKY is 19 + 11 + 25 = 55 =
;; t10. If the word value is a triangle number then we shall call the
;; word a triangle word.

;; Using words.txt (right click and 'Save Link/Target As...'), a 16K
;; text file containing nearly two-thousand common English words, how
;; many are triangle words?




;; test if a number is a triangle number.  Rather than computing all the numbers
;; each time, I thought I'd try a closure over a lazy seq of numbers.  We
;; still have to iterate over each that list for each test.

(def triangle?
  (let [;; not entirely obviouus. but (reduce + 1..n]) is the nth triangle number
        ;; reductions returns all the intermediate reduce values for that sequence,
        ;; which is the triangle sequence
        triangle-seq
        (reductions + (drop 1 (range)))]

    (fn [num-to-test]
      ;; loop through the triangle sequence until we find or exceept
      ;; the number to test.  the drop-while version didn't seem any
      ;; nicer, so I kept this code
      (loop [[triangle-num & rest-nums] triangle-seq]
        (if (= num-to-test triangle-num)
          true
          (if (> triangle-num num-to-test)
            false
            (recur rest-nums)))))))


;; the obviously better implementation - I can test that this returns
;; the right values, but I didn't do the math to verify that we only
;; ever need to test the sqrt.  
(defn better-triangle?
  ([num] (better-triangle? num (int (Math/sqrt (* 2 num)))))

  ([num guess]
       (= (* 2 num) (* guess (inc guess)))))


(defn remove-quotes [txt]
  (last (re-matches #"\"(.*)\"" txt)))

;; read a file of lines of number into a vector of vectors
(defn read-file [filename]
  (let [wordtxt (slurp filename)]
     (map remove-quotes (str/split wordtxt #","))))

(defn letter-value [letter]
  (inc (- (int letter) (int \A))))

(defn word-value [word]
  (reduce + (map letter-value word)))

(defn answer
  ([] (answer (read-file "src/euler/words.txt")))

  ([words]
     (count
      (->> words
           (map word-value)
           (filter triangle?)))))
