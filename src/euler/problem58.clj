(ns euler.problem58
  (:use [euler.problem35 :only [prime?]]))

;; Starting with 1 and spiralling anticlockwise in the following way,
;; a square spiral with side length 7 is formed.

;; 37 36 35 34 33 32 31
;; 38 17 16 15 14 13 30
;; 39 18  5  4  3 12 29
;; 40 19  6  1  2 11 28
;; 41 20  7  8  9 10 27
;; 42 21 22 23 24 25 26
;; 43 44 45 46 47 48 49

;; It is interesting to note that the odd squares lie along the bottom
;; right diagonal, but what is more interesting is that 8 out of the
;; 13 numbers lying along both diagonals are prime; that is, a ratio
;; of 8/13 62%.

;; If one complete new layer is wrapped around the spiral above, a
;; square spiral with side length 9 will be formed. If this process is
;; continued, what is the side length of the square spiral for which
;; the ratio of primes along both diagonals first falls below 10%?


;; return the diagonals for the level of sides n - assumes n is valid
(defn level [n]
  (if (= n 1)
    [1]
    (let [start
          (* (- n 2) (- n 2))]
      (take 4 (drop 1 (iterate (partial + (dec n)) start))))))

;; lazily solve the problems for increasing side lengths
;; given a side length, and n=number of primes, d=total # of numbers on diagonal
;; note to readers: initial solution used a ratio, and this second version use
;; n/d calling back to the numerator and denominator
(defn solve-seq [[len n d]]
  (lazy-seq
   (cons [len n d]
         (let [next-len (+ len 2)
               nums (level next-len)
               primes (count (filter prime? nums))]
           (solve-seq [next-len (+ n primes) (+ d 4)])))))


(defn solve []
  (->> ;; start the solution at side len=1, n=0 primes out of d=1 numbers
       (solve-seq [1 0 1])
       ;; drop the first solution for len=1, since 0/1 < 10 %
       (drop 1)
       ;; continue dropping solutions until n <= 10% of d
       (drop-while (fn [[_ n d]] 
                     (>= (* n 10) d)))
       ;; take the solution from the first matching row
       (first)
       ;; and take the first item, from that list, which is the side length
       (first)))