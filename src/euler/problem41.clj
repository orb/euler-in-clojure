(ns euler.problem41
  (:require [clojure.math.combinatorics :as comb])
  (:use [euler.problem35 :only [prime?]]))

;; We shall say that an n-digit number is pandigital if it makes use
;; of all the digits 1 to n exactly once. For example, 2143 is a
;; 4-digit pandigital and is also prime.

;; What is the largest n-digit pandigital prime that exists?


;; now pandigital supports numbers and strings, and works for all 1-9 pandigital
;; turned out I didn't use it, but I'll keep it here for later problems
(defn pandigital? [num]
  (let [numstr (str num)]
    (= (take (count numstr) [\1 \2 \3 \4 \5 \6 \7 \8 \9 :nope]) 
       (sort numstr))))

;; convert a list of digits into the equivalent numbers
(defn tonum [digits]
  (reduce #(+ %2 (* 10 %1)) digits))

;; return a seq of all the pandigital numbers of length n
;; no guaranteed ordering
(defn pandigitalseq [n]
  (map tonum (comb/permutations (range 1 (inc n)))))

(defn answer
  ([] (answer 9))
  
  ([panlen]
     (let [;; try and find an answer of length panlen
           ;; reduce using max since the numbers are unsorted,
           ;; using -1 as a not-found sentinel
           found
           (reduce max
                   -1
                   (filter prime? (pandigitalseq panlen)))]       

       ;; if we found an answer, return it.  Otherwise recurse. 
       (if (pos? found)
         found
         (answer (dec panlen))))))




