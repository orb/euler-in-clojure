(ns euler.problem49
  (:use [euler.problem7 :only [primeseq]]))

;; The arithmetic sequence, 1487, 4817, 8147, in which each of the
;; terms increases by 3330, is unusual in two ways: (i) each of the
;; three terms are prime, and, (ii) each of the 4-digit numbers are
;; permutations of one another.

;; There are no arithmetic sequences made up of three 1-, 2-, or
;; 3-digit primes, exhibiting this property, but there is one other
;; 4-digit increasing sequence.

;; What 12-digit number do you form by concatenating the three terms
;; in this sequence?


;; this is our inital sequence of prime numbers
;; all primes between 1000 and 10000
(def prime-4digits
  (->> (primeseq)
       (drop-while #(<= % 1000))
       (take-while #(< % 10000))))

;; given a number, return a key such that any number with the same
;; permutation of digits will have the same key.  specifically,
;; return a string of the sorted digits.
(defn sorted-digit-key [n]
  (apply str (sort (str n))))

;; given a list of primes (any number, really),
;; return a list of every ordered 3-pair where
;; whose values are equidistant
(defn find-3primes [primes]
  (for [p1 primes
        p2 primes
        :when (< p1 p2)
        p3 primes
        :when (and (< p2 p3) (= (- p3 p2) (- p2 p1)))]
    [p1 p2 p3]))


;; the solution here should probably be generalized just a bit. I'm very happy
;; with the logic, but the sequence of operations strikes me as a little awkward
(defn answer []
  (->> ;; start with all the 4-digit priems
       prime-4digits
       ;; group in to buckets by their sortkey
       (group-by sorted-digit-key)
       ;; get the values only - not interested in the keys
       (vals)
       ;; run each bucket through a function that returns increasing sequences
       ;; of 3-pairs that are equidistant as a list of 3-pairs
       (map find-3primes)
       ;; concat them all together to get to a single list of 3-pairs
       (apply concat)
       ;; form a string from the values, which are already ordered
       (map #(apply str %))
       ;; and optionally, filter the uninteresting one from the problem statement
       #_(filter #(not= % "148748178147"))
       #_(first)))