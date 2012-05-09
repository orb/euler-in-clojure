(ns euler.problem37
  (:use [euler.problem35 :only [prime?]]))

;; The number 3797 has an interesting property. Being prime itself, it
;; is possible to continuously remove digits from left to right, and
;; remain prime at each stage: 3797, 797, 97, and 7. Similarly we can
;; work from right to left: 3797, 379, 37, and 3.

;; Find the sum of the only eleven primes that are both truncatable
;; from left to right and right to left.

;; NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.


(defn reverse-num [n]
  "reverse a number through it's string representation.  must fit in long"
  (Long/parseLong (apply str (reverse (str n)))))


(defn truncate-right [n]
  (if (< n 10)
    (list n)
    (cons n (lazy-seq (truncate-right (quot n 10))))))

;; not general purpose - numbers ending with zero are wrong
;; however, no numbers we are testing will contain zeros
(defn truncate-left [n]
  (map reverse-num (truncate-right
                    (reverse-num n))))

(defn extend-right [n]
  "extend number by adding all digits to the right"
  (map #(+ (* 10 n) %)
       (range 0 10)))


(defn answer []
  (loop [found []
         frontier (filter prime? (range 2 10))]
    (if (= 11 (count found))
      ;; return the sum
      (reduce + found)
      
      (let [to-test
            ;; the next item to test
            (first frontier)

            next-found
            ;; if we have a solution, add it to the solutions for the
            ;; next iteration
            (if (and (> to-test 10)
                     (every? prime? (truncate-left to-test)))
              (conj found to-test)
              found)

            ;; extend the frontier with all primes reachable by
            ;; adding a digit to the corrent prime
            next-frontier
            (concat (rest frontier)
                    (filter prime? (extend-right to-test)))]

        (recur next-found next-frontier)))))