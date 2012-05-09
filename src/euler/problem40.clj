(ns euler.problem40)

;; An irrational decimal fraction is created by concatenating the
;; positive integers:

;; 0.123456789101112131415161718192021...

;; It can be seen that the 12th digit of the fractional part is 1.

;; If dn represents the nth digit of the fractional part, find the
;; value of the following expression.

;; d1  d10  d100  d1000  d10000  d100000  d1000000

(defn digits [num]
  "a seq of digits in a number"
  (map #(- (int %) (int \0)) (str num)))

(defn digit-seq [nums]
  "a seq of the digits in all the numbers in a range"
  (mapcat digits nums))

;; if places is an increasing list of indexes into the items seq
;; select-items returns the items at each of those positions
(defn select-items [places items]
  (when (and (seq places) (seq items))
    (let [to-drop (first places)          
          next-items (drop to-drop items)
          next-places (map #(- % to-drop) (rest places))]
      (lazy-seq (cons (first next-items)
                      (select-items next-places next-items))))))

;; final solution was to create a sequence of all the digits
;; and take the appropriate items.  this solution computes each and every
;; digit. I thought I might end up needing to be somewhat more clever about
;; skipping places, but it turns out that as inefficient as this solution is, it is
;; more than fast enough for the input range.
(defn answer
  ([] (answer
       (take 7 (iterate #(* 10 %) 1))))

  ([places]
     (reduce *
             (select-items (map dec places) ;; 0-based will make it easier
                           (digit-seq (rest (range)))))))