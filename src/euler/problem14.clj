(ns euler.problem14)

;;The following iterative sequence is defined for the set of positive integers:

;;n  n/2 (n is even)
;;n  3n + 1 (n is odd)

;;Using the rule above and starting with 13, we generate the following sequence:

;;13  40  20  10  5  16  8  4  2  1

;;It can be seen that this sequence (starting at 13 and finishing at
;;1) contains 10 terms. Although it has not been proved yet (Collatz
;;Problem), it is thought that all starting numbers finish at 1.

;;Which starting number, under one million, produces the longest chain?

;;NOTE: Once the chain starts the terms are allowed to go above one million.

(defn next-chain-val [n]
  (if (even? n)
    (/ n 2)
    (inc (* 3 n))))

(defn chain [n]
  (if (= n 1)
    (list 1)
    (lazy-seq
     (cons n
           (chain (next-chain-val n))))))

(defn answer
  ([] (answer 1000000))
  
  ([n]
     (let [chain-seq
           (for [i (range 1 n)]
             [i (count (chain i))])
        
           chain-reduce
           (fn [[_ l1 :as c1] [_ l2 :as c2]]
             (if (> l1 l2)
               c1 c2))]
       (first (reduce chain-reduce chain-seq)))))



