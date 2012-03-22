(ns euler.problem9
  (:require [clojure.math.numeric-tower :as num]))

;;A Pythagorean triplet is a set of three natural numbers, a  b  c, for which,

;;a2 + b2 = c2
;;For example, 32 + 42 = 9 + 16 = 25 = 52.

;;There exists exactly one Pythagorean triplet for which a + b + c = 1000.
;;Find the product abc.


(def target-sum 1000)

(defn answer []
  (first
   (for [x (range 0 target-sum)
         y (range (inc x) (- target-sum x))
         :let [z (- target-sum x y)]
         :when (< x y z)
         :when (= (* z z) (+ (* x x) (* y y)))]
     (* x y z))))



