(ns euler.problem21
  (:use [euler.problem12 :only [factors]]))

;;Let d (n) be defined as the sum of proper divisors of n (numbers
;;less than n which divide evenly into n).

;;If d (a) = b and d (b) = a, where a b, then a and b are an amicable
;;pair and each of a and b are called amicable numbers.

;;For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20,
;;22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of
;;284 are 1, 2, 4, 71 and 142; so d(284) = 220.

;;Evaluate the sum of all the amicable numbers under 10000.


;; factors includes n, so we subtract that out
(defn sum-proper-factors [n]
  (- (reduce + (factors n))
     n))

;; create map for all d values
(defn d-map [n]
  (let [d-reduce
        (fn [acc n]
          (if (contains? acc n)
            acc
            (assoc acc n (sum-proper-factors n))))]
    (reduce d-reduce {} (range 1 (inc n)))))


;; collect all the values, filter the amicable pairs and sum
(defn answer
  ([] (answer 10000))
  ([n]
     (let [d-vals
           (d-map n)

           amicable?
           (fn [a]
             (let [b (d-vals a)]
               (and (<= b n)
                    (not= a b)
                    (= a (d-vals b)))))

           amicables (filter amicable?
                             (range 1 (inc n)))]

       (reduce + amicables))))
