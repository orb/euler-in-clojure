(ns euler.problem57)

;; It is possible to show that the square root of two can be expressed as an infinite continued fraction.

;; 2 = 1 + 1/ (2 + 1/ (2 + 1/ (2 + ...))) = 1.414213...

;; By expanding this for the first four iterations, we get:

;; 1 + 1/2 = 3/2 = 1.5
;; 1 + 1/ (2 + 1/2) = 7/5 = 1.4
;; 1 + 1/ (2 + 1/ (2 + 1/2)) = 17/12 = 1.41666...
;; 1 + 1/ (2 + 1/ (2 + 1/ (2 + 1/2))) = 41/29 = 1.41379...

;; The next three expansions are 99/70, 239/169, and 577/408, but the
;; eighth expansion, 1393/985, is the first example where the number
;; of digits in the numerator exceeds the number of digits in the
;; denominator.

;; In the first one-thousand expansions, how many fractions contain a
;; numerator with more digits than denominator?



;; return a lazy sequence in successive iteration of the approximation
(defn sqrt2-approximations []
  (iterate #(+ 1 (/ 1 (+ 1 %)))
           1))

;; count the digits in a number by string conversion
(defn count-digits [n]
  (count (str n)))

;; are there more digits in the numerator than the denominator?
;; only valid for ratios
(defn numerator-digits-dominate? [n]
    (> (count-digits (numerator n))
       (count-digits (denominator n))))

;; it's not clear from the problem whether the first term "1" is included in the "first one
;; thousand expansions".  It doesn't affect the solution here, which does not include the
;; initial 1, but only because it made the numerator-digits test easier when non-ratios were
;; not includeded
(defn solve
  ([]
     (solve 1000))

  ([n]
     (->> (sqrt2-approximations) 
          (drop 1)
          (take n)
          (filter numerator-digits-dominate?)
          (count))))