(ns euler.problem33)

;; The fraction 49/98 is a curious fraction, as an inexperienced
;; mathematician in attempting to simplify it may incorrectly believe
;; that 49/98 = 4/8, which is correct, is obtained by cancelling the
;; 9s.

;; We shall consider fractions like, 30/50 = 3/5, to be trivial
;; examples.

;; There are exactly four non-trivial examples of this type of
;; fraction, less than one in value, and containing two digits in the
;; numerator and denominator.

;; If the product of these four fractions is given in its lowest
;; common terms, find the value of the denominator.


;; convert 2 digits to a 2-digit number
(defn num2 [d1 d2]
  (+ (* 10 d1) d2))



(defn answer []
  (let [fractions

        ;; don't include 0 because it's "trivial", since "trivial"
        ;; wasn't defined, it's not clear to me what cases to
        ;; consider.  this code only considers fractions of the form
        ;; AB/BC when it equals A/C and A != B != C.  whether this is
        ;; a correct interpretation of "trivial" or one that just
        ;; happens to produce the desired answers is unclear
        (for [d1 (range 1 10) ;; A
              d2 (range 1 10) ;; B
              d3 (range 1 10) ;; C

              :let [num1 (num2 d1 d2) ;;AB
                    den1 (num2 d2 d3) ;;BC
                    num2 d1  ;; A
                    den2 d3] ;; C

              :when (and (not= d1 d2) (not= d2 d3)
                         ;; ratio math check is exact
                         (= (/ num1 den1) (/ num2 den2)))]
          (/ num2 den2))]

    ;; clojure supports auto-reduced ratios, so we can just multiply
    ;; up the results and ask for the denominator
    (denominator (reduce * fractions))))


