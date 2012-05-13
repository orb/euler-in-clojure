(ns euler.problem47)


;; The first two consecutive numbers to have two distinct prime factors are:

;; 14 = 2  7
;; 15 = 3  5

;; The first three consecutive numbers to have three distinct prime factors are:

;; 644 = 2²  7  23
;; 645 = 3  5  43
;; 646 = 2  17  19.

;; Find the first four consecutive integers to have four distinct
;; primes factors. What is the first of these numbers?


;; return all the prime factors of n
(defn distinct-factors [n] 
  (loop [i 2 n n factors #{}]
    (cond
     (= n 1)
     factors

     (> (* i i) n)
     (conj factors n)

     (zero? (mod n i))
     (recur i (quot n i) (conj factors i))

     :else
     (recur (inc i) n factors))))


(defn answer 
  ([]
     (answer 4 4))

  ;; this was really straight forward.
  ;; the main loop tracks the sequence of numbers
  ;; to test and the trailing count of how many consecutive
  ;; items matched the test.  When we hit the target number,
  ;; look back to the first item in the sequence and return it
  ([num-factors-target seen-target]
     (loop [seen-count 0
            [num & nums] (iterate inc 2)]

       (if (= seen-count seen-target)
         (- num seen-count)

         (let [new-count
               (if (= num-factors-target
                      (count (distinct-factors num)))
                 (inc seen-count)
                 0)]
           (recur new-count nums))))))

