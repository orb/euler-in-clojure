(ns euler.problem5
  (:require [clojure.math.numeric-tower :as num]))


;;2520 is the smallest number that can be divided by each of the
;;numbers from 1 to 10 without any remainder.

;;What is the smallest positive number that is evenly divisible by all
;;of the numbers from 1 to 20?

(defn gcd-reduce [x y]
  (/ (* x y) (math/gcd x y)))

(defn answer
  ([] (answer 20))
  ([n] (reduce gcd-reduce (range 1 (inc n)))))



