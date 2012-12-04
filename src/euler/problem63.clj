(ns euler.problem63 
  (require [clojure.math.numeric-tower :as num]))

;; The 5-digit number, 16807=7^5, is also a fifth power. Similarly, the
;; 9-digit number, 134217728=8^9, is a ninth power.

;; How many n-digit positive integers exist which are also an nth power?


;; count the number of digits of n, by string conversion
(defn num-digits [n]
  (count (str n)))

;; test the number of digits in 9^n, returns true if it is at least n
(defn test-9-limit [n]
  (>= (num-digits (num/expt 9 n)) n))

(defn solve []
  (let [;; 10^n always has n+1 digits, 9^n has at most n digits
        ;; when 9^n has <n digits, we've exhausted the search space
        ;; as multiplying a number by 9 will never add for than 1 digit
        ;; and we'll never reach n digits
        n-range
        (take-while test-9-limit (range))

        ;; by the same logic, the range of m is 1..9, as no m >=10 can be a solution
        m-range
        (range 1 10)

        ;; within the ranges of n m, find all the numbers
        ;; where m^n has n digits
        found-numbers
        (for [n n-range
              m m-range
              :let [mn (num/expt m n)]
              :when (= n (num-digits mn))]
          mn)]

    ;; each number should be distinct (only produced by one nth root)
    ;; so just count the solutions
    (count found-numbers)))

