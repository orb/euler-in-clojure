(ns euler.problem46
  (:use [euler.problem7 :only [primeseq]])
  (:use [euler.problem35 :only [prime?]]))

;; It was proposed by Christian Goldbach that every odd composite
;; number can be written as the sum of a prime and twice a square.

;; 9 = 7 + 2*1^2
;; 15 = 7 + 2*2^2
;; 21 = 3 + 2*3^2
;; 25 = 7 + 2*3^2
;; 27 = 19 + 2*2^2
;; 33 = 31 + 2*1^2

;; It turns out that the conjecture was false.

;; What is the smallest odd composite that cannot be written as the
;; sum of a prime and twice a square?

(defn perfect-square? [n]
  (let [sqrt (int (Math/sqrt n))]
    (= n (* sqrt sqrt)))) 

;; decided to test in the positive - that is verify tests that the hypothesis
;; holds for the the given number n, assuming that n is an odd composite coming in
(defn verify
  ([n]
     ;; test that there is some prime for which the condition holds
     (some #(verify n %)
           (take-while #(< % n) (primeseq))))

  ;; tests n for the given p - which we can do with a constant time test
  ;; rather than a search
  ([n p]
     (perfect-square? (/ (- n p)  2))))

(defn answer []
  (first
   (->> 
        (iterate #(+ 2 %) 3)
        ;; remove the primes
        (remove prime?)
        ;; and remove the numbers that pass verify.
        ;; anything 
        (remove verify))))