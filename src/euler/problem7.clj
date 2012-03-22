(ns euler.problem7
  (:require [clojure.math.numeric-tower :as num]))

;;By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we
;;can see that the 6th prime is 13.

;;What is the 10 001st prime number?

(defn factor? [f n]
  (zero? (mod n f)))

(defn primeseq
  ([]
     (primeseq [] 2))
  ([prev n]
     (if (some #(factor? % n) prev)
       (recur prev (inc n))
       (lazy-seq
        (cons n
              (primeseq (conj prev n) (inc n)))))))

(defn answer []
  (->> (primeseq)
       (drop 10000)
       (first)))



