(ns euler.problem53
  (:use [euler.problem20 :only [factorial]])
  (:require [clojure.math.numeric-tower :as num])
  (:require [clojure.math.combinatorics :as comb]))


;; There are exactly ten ways of selecting three from five, 12345:

;; 123, 124, 125, 134, 135, 145, 234, 235, 245, and 345

;; In combinatorics, we use the notation, 5C3 = 10.

;; In general,

;; nCr = n! / r! (n-r)!
;; ,where r <=  n, n! = n x (n-1) x ... x 3 x 2 x 1, and 0! = 1.

;; It is not until n = 23, that a value exceeds one-million: 23C10 = 1144066.

;; How many, not necessarily distinct, values of nCr, for 1 n 100, are
;; greater than one-million?

(defn n-choose-r [n r]
  (/ (factorial n) (*' (factorial r) (factorial (- n r)))))


;; really boring solution - could obviously be more efficient with the
;; calculations. 
(defn
  solve
  ([]
     (solve 100))

  ([max]
     (count
      (for [n (range 1 (inc 100))
            r (range 1 (inc n))
            :let [choices (n-choose-r n r)]
            :when (>= choices 1000000)]
        [n r]))))