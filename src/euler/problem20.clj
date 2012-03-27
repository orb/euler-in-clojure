(ns euler.problem20)

;;n! means n  (n  1)  ...  3  2  1

;;For example, 10! = 10 9 ...  3 2 1 = 3628800, and the sum of the
;;digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.

;;Find the sum of the digits in the number 100!


(defn factorial [n]
  (reduce *' (range 1 (inc n))))

(defn sum-digits [n]
  (if (= n 0)
    0
    (+ (mod n 10) (sum-digits (quot n 10)))))

(defn answer
  ([] (answer 100))
  ([n]  (sum-digits (factorial n))))
