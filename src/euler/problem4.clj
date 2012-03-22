(ns euler.problem4
  (require
   clojure.string))


;;A palindromic number reads the same both ways. The largest
;;palindrome made from the product of two 2-digit numbers is 9009 = 91
;;99.

;;Find the largest palindrome made from the product of two 3-digit numbers.


(defn palnum? [n]
  (let [numstr (.toString n)]
    (= numstr (clojure.string/reverse numstr))))


(defn answer []
  (let [num3digits (range 100 1000)
        prod2nums (for [n1 num3digits n2 num3digits :when (< n1 n2)] (* n1 n2))
        palnums (filter palnum? prod2nums)]
    (reduce max palnums)))



