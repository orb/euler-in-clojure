(ns euler.problem34)

;; 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.

;; Find the sum of all numbers which are equal to the sum of the
;; factorial of their digits.

;; Note: as 1! = 1 and 2! = 2 are not sums they are not included.

(defn factorial [n]
  (cond
   (< n 1)
   1
   
   :else
   (* n (factorial (dec n)))))

(def factmap
  (reduce into {} (for [n (range 0 10)] {n (factorial n)})))


(defn numberseq [n]
  (if (= n 0)
    []
    (conj (numberseq (quot n 10))
          (rem n 10))))

(defn factorsum [n]
  (reduce + (map factmap (numberseq n))))

;; upper bound based on 9999999 > 7*9! 
(defn answer []
  (reduce +
          (filter #(= % (factorsum %)) (range 10 9999999))))

