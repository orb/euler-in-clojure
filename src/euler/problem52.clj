(ns euler.problem52)

;; It can be seen that the number, 125874, and its double, 251748,
;; contain exactly the same digits, but in a different order.

;; Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x,
;; and 6x, contain the same digits.


;; returns true if the all the numbers in the sequence have the same digits
(defn same-digits? [numbers]
  (apply = (map sort (map str numbers))))

;; returns a sequence of n multiples of x
(defn n-multiples [n x]
  (take n (iterate (partial + x) x)))

(defn solve
  ([]
     (solve 6))

  ([n]
     (->> (range)
          ;; start at 1
          (drop 1)
          ;; select the numbers where the first n multiples have
          ;; the same digit
          (filter #(same-digits? (n-multiples n %)))
          ;; and return the first item
          first)))
