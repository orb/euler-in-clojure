(ns euler.problem56
  (require [clojure.math.numeric-tower :as num]))


;; A googol (10100) is a massive number: one followed by one-hundred
;; zeros; 100100 is almost unimaginably large: one followed by
;; two-hundred zeros. Despite their size, the sum of the digits in
;; each number is only 1.

;; Considering natural numbers of the form, a^b, where a, b < 100,
;; what is the maximum digital sum?



(defn sum-digits [num]
  (loop [num num acc 0]
    (if (= num 0)
      acc
      (recur (quot num 10) (+' acc (mod num 10))))))


(defn solve
  ([] (solve 100))

  ([n]
     (reduce max
             (for [a (range 1 n)
                   b (range 1 n)]               
               (sum-digits (num/expt a b))))))


