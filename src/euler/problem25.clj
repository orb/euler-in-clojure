(ns euler.problem25)

;; The Fibonacci sequence is defined by the recurrence relation:

;; Fn = Fn1 + Fn2, where F1 = 1 and F2 = 1.
;; Hence the first 12 terms will be:

;; F1 = 1
;; F2 = 1
;; F3 = 2
;; F4 = 3
;; F5 = 5
;; F6 = 8
;; F7 = 13
;; F8 = 21
;; F9 = 34
;; F10 = 55
;; F11 = 89
;; F12 = 144
;; The 12th term, F12, is the first term to contain three digits.

;; What is the first term in the Fibonacci sequence to contain 1000 digits?


;; from problem2, but with arbitrary precision
(defn fibseq [n1 n2]
  (lazy-seq (cons n1 (fibseq n2 (+' n1 n2)))))


;; clojure does have a good unbounded range that starts at a non-zero
(defn range-from [n]
  (range n Double/POSITIVE_INFINITY 1))

(defn count-digits [n]
  (count (.toString n)))

(defn answer
  ([]
     (answer 1000))

  ([n]
     (->> (map vector
               (range-from 1)
               (fibseq 1 1))

          (drop-while #(< (count-digits (second %)) n))
          (first) ;; first pair that isn't dropped
          (first) ;; extract index
          )))

