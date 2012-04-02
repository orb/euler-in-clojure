(ns euler.problem30
  (:require [clojure.math.numeric-tower :as num]))


;; Surprisingly there are only three numbers that can be written as
;; the sum of fourth powers of their digits:

;; 1634 = 1^4 + 6^4 + 3^4 + 4^4
;; 8208 = 8^4 + 2^4 + 0^4 + 8^4
;; 9474 = 9^4 + 4^4 + 7^4 + 4^4

;; As 1 = 1^4 is not a sum it is not included.

;; The sum of these numbers is 1634 + 8208 + 9474 = 19316.

;; Find the sum of all the numbers that can be written as the sum of
;; fifth powers of their digits.

;; sum the digits of n using the given power
(defn sum-power [n power]
  (if (= n 0)
    0
    (+ (num/expt (rem n 10) power)
       (sum-power (quot n 10) power))))


;; we need some sort of upper bound
;; the largest sum-power for an n-digit number will be sum-power of 10^n-1
;; when the maximum sum can never reach that value, then we know we have
;; an upper bound.  It may not be the best upper bound, but it is an upper
;; bound
(defn find-max-for [n]
  (loop [max 99]
    (let [max-sum (sum-power max n)]
      (if (> max max-sum)
        max
        (recur (+ 9 (* max 10)))))))


(defn answer
  ([] (answer 5))

  ([n]
     (reduce +
             (filter #(= % (sum-power % n))
                     (range 10 (find-max-for n))))))
