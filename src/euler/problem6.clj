(ns euler.problem6
  (:require [clojure.math.numeric-tower :as num]))

;;The sum of the squares of the first ten natural numbers is,

;;12 + 22 + ... + 102 = 385
;;The square of the sum of the first ten natural numbers is,

;;(1 + 2 + ... + 10)2 = 552 = 3025 Hence the difference between the
;;sum of the squares of the first ten natural numbers and the square
;;of the sum is 3025 385 = 2640.

;;Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.


(defn sq [n] (* n n))

(defn answer
  ([] (answer 100))
  ([n]
     (let [numbers
           (range 1 (inc n))
           sumsq (reduce + (map sq numbers))
           sqsum (sq (reduce + numbers))]
       (num/abs (- sumsq sqsum)))))



