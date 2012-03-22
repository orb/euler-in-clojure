(ns euler.problem3)

;;The prime factors of 13195 are 5, 7, 13 and 29.

;;What is the largest prime factor of the number 600851475143 ?


(def target 600851475143)

(defn prime-factor [m n]
  (if (> (* m m)  n)
    n
    (if (zero? (rem n m))
      (recur m (quot n m))
      (recur (inc m) n))))

(defn answer [] (prime-factor 2 target))

