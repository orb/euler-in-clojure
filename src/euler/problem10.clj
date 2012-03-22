(ns euler.problem10
  (:use [euler.problem7 :only [primeseq]])
  (:require [clojure.math.numeric-tower :as num]))


;The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

;Find the sum of all the primes below two million.


(defn sum-primes-below [n]
  (reduce +
          (take-while #(< % n) (primeseq)))) 

;; very slow
;; Elapsed time: 914149.079 msecs
(defn answer []
  (sum-primes-below 2000000))
