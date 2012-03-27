(ns euler.problem23
  (:use [euler.problem21 :only [sum-proper-factors]]))

;; A perfect number is a number for which the sum of its proper
;; divisors is exactly equal to the number. For example, the sum of
;; the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which
;; means that 28 is a perfect number.

;; A number n is called deficient if the sum of its proper divisors is
;; less than n and it is called abundant if this sum exceeds n.

;; As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the
;; smallest number that can be written as the sum of two abundant
;; numbers is 24. By mathematical analysis, it can be shown that all
;; integers greater than 28123 can be written as the sum of two
;; abundant numbers. However, this upper limit cannot be reduced any
;; further by analysis even though it is known that the greatest
;; number that cannot be expressed as the sum of two abundant numbers
;; is less than this limit.

;; Find the sum of all the positive integers which cannot be written
;; as the sum of two abundant numbers.



;; test if a number is abundant, using factor code from previous problems
(defn abundant? [n]
  (> (sum-proper-factors n) n))

;; determine if n is the sum of any two numbers in by
(defn summable? [n by]
  (some #(contains? by (- n %)) by))


;; collect the abundant numbers and brute for the range
;; looking for summable pairs for each value

(defn answer
  ([]
     (answer 28123))

  ([n]
     (let [bound
           (range 1 (inc n))

           abundant-set
           (into #{} (filter abundant? bound))

           not-abundant?
           (complement #(summable? % abundant-set))]

       ; I really love it when a problem abstracts this nicely
       (reduce +
               (filter not-abundant? bound)))))

