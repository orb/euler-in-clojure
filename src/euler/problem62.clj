(ns euler.problem62
  (require [clojure.math.numeric-tower :as num]))
;; The cube, 41063625 (345^3), can be permuted to produce two other
;; cubes: 56623104 (384^3) and 66430125 (405^3). In fact, 41063625 is
;; the smallest cube which has exactly three permutations of its
;; digits which are also cube.

;; Find the smallest cube for which exactly five permutations of its
;; digits are cube.


(defn cube [n]
  (*' n n n))

;; approximate cube-root, doesn't return exact integer values
(defn cube-root [n]
  (num/expt n (/ 1 3)))

;; a key function that returns a index string for a number n
;; with digits sorted 
(defn digit-order-key [n]
  (->> (str n)
       (sort)
       (apply str)))

;; returns a filter function for a map entries that have exactly n
;; items in the value
(defn kv-has-n-values [n]
  (fn [[key values]]
    (= n (count values))))

;; find the min of possible-nil seq, returns min or nil
(defn nillable-min [vals]
  (when (seq vals)
    (apply min vals)))

;; the core of the solution - solve the problem for n values in the fixed
;; range from start to end.  will return the value or nil.
(defn solve-for-range [n start end]
  (->> ;; start with our range
       (range start end)
       ;; sort into groups according to digit order of the cube of the number       
       (group-by #(digit-order-key (cube %)))
       ;; select the groupings that have at least n values
       (filter (kv-has-n-values n))
       ;; for each grouping, return the cube of the number in that grouping
       (map #(cube (apply min (second %))))
       ;; because a range might have multiple solutions, return the minimum
       (nillable-min)))

;; solve the problem for n values for any number whose cube has the given
;; number of digits
(defn solve-for-digits [n digits]
  (let [lower (int (num/floor (cube-root (num/expt 10 digits))))
        upper (int (num/ceil (cube-root (num/expt 10 (inc digits)))))]

    ;; compute lower and upper bounds for the given number of digits and
    ;; solve.  note that lower and upper are not exact bounds, but should
    ;; maintain: lower <= true lower bound <= true upper bound <= upper
    ;; there is no harm, besides doing too much work, of using a larger
    ;; than necessary upper bound
    (solve-for-range n lower upper)))

;; return the first non-nil value from a sequence
(defn first-non-nil [vals]
  (first (keep identity vals)))

;; return a lazy-seq of the numbers n and higher.  the values won't be
;; chunked, which is important for efficiency of the solution
(defn numbers-from [n]
  (iterate inc n))

(defn solve
  ([]
     (solve 5))
  
  ([n]
     (first-non-nil
      (map #(solve-for-digits n %)
           (numbers-from 1)))))
