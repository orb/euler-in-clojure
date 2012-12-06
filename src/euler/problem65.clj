(ns euler.problem65)

;; this produces the terms of the e sequence -
;; 2 1 - 2 1 1 - 4 1 1 - 6 1 1 - ...
(defn e-seq
  ([]
     (concat [2 1]
             (e-seq 2))) 

  ([n]
     (lazy-cat [n 1 1]
               (e-seq (+ 2 n)))))

;; given a sequence of a finite length, evaluate the repeating fraction it
;; represents
(defn eval-repeating-fraction [ns]
  (let [next-term
        (fn [x y]
          (+ y (/ 1 x)))]
    (reduce next-term (reverse ns))))


(defn sum-of-digits [n]
  (loop [n n sum 0]
    (if (= n 0)
      sum
      (recur (quot n 10)
             (+ sum (rem n 10))))))

(defn format-answer [r]
  (sum-of-digits (numerator r)))

(defn solve
  ([] (solve 100))
  ([n]
     (->> (e-seq)
         (take n)
         eval-repeating-fraction
         format-answer)))