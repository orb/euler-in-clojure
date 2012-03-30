(ns euler.problem26)

;; A unit fraction contains 1 in the numerator. The decimal
;; representation of the unit fractions with denominators 2 to 10 are
;; given:

;; 1/2= 0.5
;; 1/3= 0.(3)
;; 1/4= 0.25
;; 1/5= 0.2
;; 1/6= 0.1(6)
;; 1/7= 0.(142857)
;; 1/8= 0.125
;; 1/9= 0.(1)
;; 1/10= 0.1

;; Where 0.1(6) means 0.166666..., and has a 1-digit recurring
;; cycle. It can be seen that 1/7 has a 6-digit recurring cycle.

;; Find the value of d 1000 for which 1/d contains the longest
;; recurring cycle in its decimal fraction part.



;; lazy sequences of the remainders when dividing out 1/n
(defn div-remainders [n]
  (iterate #(rem (* 10 %) n) 1))

;; this is the first pass of this function - it computes the digits and
;; the remainders.  Since this problem doesn't need the digits, I refactored
;; to the simpler div-remainders

;; (defn div-with-remainders
;;   ([n]
;;      (digits 10 n))
;;   ([n r]
;;      (lazy-seq (cons
;;                 [(quot r n) r]
;;                 (digits n (* 10 (rem r n)))))))

;; whenever the remainder repeats, the following digits will repeat
;; accumuator stores up remainder -> pos, so we can quickly test
;; if we've seen the value and return the difference in positions, which
;; is the cycle length

(defn repeat-length [n]
  (loop [[first-remainder & rest-remainder] (div-remainders n)
         pos 1
         acc {}]
    (if (acc first-remainder)
      (- pos (acc first-remainder))
      (recur rest-remainder
             (inc pos)
             (assoc acc first-remainder pos)))))

(defn answer
  ([]
     (answer 1000))

  ;; max-key is great helper function here - though it does not save
  ;; the repeat-length value-between calls. I've thus gratuitously memoize'd
  ;; the call
  ([n]
     (apply max-key
            (memoize repeat-length)
            (range 2 n))))

