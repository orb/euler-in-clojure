(ns euler.problem32)

;; We shall say that an n-digit number is pandigital if it makes use
;; of all the digits 1 to n exactly once; for example, the 5-digit
;; number, 15234, is 1 through 5 pandigital.

;; The product 7254 is unusual, as the identity, 39 186 = 7254,
;; containing multiplicand, multiplier, and product is 1 through 9
;; pandigital.

;; Find the sum of all products whose multiplicand/multiplier/product
;; identity can be written as a 1 through 9 pandigital.

;; HINT: Some products can be obtained in more than one way so be sure
;; to only include it once in your sum.



(defn pandigital? [x y z]  
  (let [str (print-str x y z)
        digits (sort str)]
    ;; print-str will insert two spaces.  rather than being fancy
    ;; here, just sort the text string and compare
    (= [\space \space \1 \2 \3 \4 \5 \6 \7 \8 \9] digits)))

;; find pandigital products for the given ranges
(defn solve [range1 range2]
  (for [x range1
        y range2
        :let [z (* x y)]
        :when (pandigital? x y z)] z))

(defn answer []
  (reduce +
          ;; put in a set to remove dups
          (into #{}
                ;; potential solutions should have the form
                ;; N x NNNN = NNNNN
                ;; NN x NNN = NNNNN
                (concat (solve (range 1 (inc 9)) (range 1234 (inc 8765)))
                        (solve (range 12 (inc 98)) (range 123 (inc 987)))))))


