(ns euler.problem27
  (:use [euler.problem7 :only [primeseq]]))

;; Considering quadratics of the form:

;; n² + an + b, where |a| < 1000 and |b| < 1000

;; where |n| is the modulus/absolute value of n
;; e.g. |11| = 11 and |-4| = 4

;; Find the product of the coefficients, a and b, for the quadratic
;; expression that produces the maximum number of primes for
;; consecutive values of n, starting with n = 0.



;; optimization #1 - save the pseq head. this should realize all
;; interesting primes once.  with optimization #2, this is less
;; critical but still useful
(def pseq (primeseq))

(defn prime? [n]
  (loop [[prime1 & primes] pseq]
    (cond
     (= n prime1) true
     (> prime1 n) false
     :else (recur primes))))

;; optimization #2 - memoize the actual prime? 
(def memo-prime? (memoize prime?))

;; counts sequential primes

;; also tried co-iterating items and pseq looking for the first item
;; not in pseq.  but the memoization caching seems to help more
(defn count-primes [items]
  (count (take-while memo-prime? items)))


;; range from -n <= i <= n
(defn range+- [n]
  (range (- n) (inc n)))

;; return a function computting n^2+an+b
(defn poly-fn [a b]
  (fn [n] (+ (* n n) (* a n) b)))

;; for a function, return a sequence evaluating
;; it for all n >= 0
(defn poly-seq [f]
  (map f (range)))


(defn third [l]
  (nth l 2))

(defn answer
  ([]
     (answer 1000))

  ([n]
     (let [num-primes
           (fn [a b]
             (count-primes (poly-seq (poly-fn a b))))

           ab-counts
           (for [a (range+- n)
                 b (take-while #(<= % n) pseq)]
             {:product (* a b) :num-primes (num-primes a b)})]

       
       (:product (apply max-key :num-primes ab-counts)))))
