(ns euler.problem16
  (:require [clojure.math.numeric-tower :as num]))

;;2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

;;What is the sum of the digits of the number 2^1000?



;; usually just do string conversion here, but let's do the numeric way for kicks
(defn sumdigits
  ([n]
     (loop [n n sum 0]
       (if (zero? n)
         sum
         (recur (quot n 10)
                (+' sum (rem n 10)))))))

(defn answer
  ([] (answer 1000))
  
  ([n]
     (sumdigits (num/expt 2 n))))



