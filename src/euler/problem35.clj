(ns euler.problem35
  (:use [euler.problem7 :only [primeseq]]))

;; The number, 197, is called a circular prime because all rotations
;; of the digits: 197, 971, and 719, are themselves prime.

;; There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17,
;; 31, 37, 71, 73, 79, and 97.

;; How many circular primes are there below one million?

(defn sqrt [n]
  (int (Math/sqrt n)))


;;; for n >= 2, test for primality
(defn prime? [n]
  (cond
   (< n 2)
   false
   
   (= n 2)
   true

   :else
   (let [factors (for [i (range 2 (inc (sqrt n)))
                       :when (zero? (mod n i))]
                   i)]
     (nil? (first factors)))))
                 

;;; generate all rotations of a text
(defn strperms [text]
  (let [source (str text text)
        len (count text)]
    (butlast                            ; filter out the last item, which is a duplication
     (map #(apply str %)
          (partition len 1 source)))))



;;; test if a number n is a circular prime
(defn circularprime? [n]
  (let [nperms (map #(Integer/parseInt %)
                    (strperms (str n)))]
    (every? prime? nperms)))


;;; a sequence of all circular primes < n
(defn circularprimeseq [n]
  (->>
   (range 2 n)
   (filter circularprime?)))

(defn answer
  ([]
     (answer 1000000))
  
  ([n]
     (count (circularprimeseq n))))