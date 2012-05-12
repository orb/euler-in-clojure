(ns euler.problem43
 (:require [clojure.math.combinatorics :as comb]))

;; The number, 1406357289, is a 0 to 9 pandigital number because it is
;; made up of each of the digits 0 to 9 in some order, but it also has
;; a rather interesting sub-string divisibility property.

;; Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this
;; way, we note the following:

;; d2d3d4=406 is divisible by 2
;; d3d4d5=063 is divisible by 3
;; d4d5d6=635 is divisible by 5
;; d5d6d7=357 is divisible by 7
;; d6d7d8=572 is divisible by 11
;; d7d8d9=728 is divisible by 13
;; d8d9d10=289 is divisible by 17

;; Find the sum of all 0 to 9 pandigital numbers with this property.


;; given all these definitions of pandigital, it's clear I need
;; one definitive version of this.
;; -- but I actually ended up not using it in any of my final solutions
(defn pandigital? [num]
  (let [numstr (str num)]
    (= [\0 \1 \2 \3 \4 \5 \6 \7 \8 \9]
       (sort numstr))))

(defn to-num [digits]
  (reduce #(+ (* 10 %1) %2) 0 digits))

(defn divisible-by?
  [f n]
  "is n evenly divisible by f?"
  (zero? (mod n f)))

(defn special? [[d1 d2 d3 d4 d5 d6 d7 d8 d9 d10]]
  "tests if the digits meet the special properties of the problem"
  (and
   (not= d1 0)
   (divisible-by? 2  (to-num [d2 d3 d4]))
   (divisible-by? 3  (to-num [d3 d4 d5]))
   (divisible-by? 5  (to-num [d4 d5 d6]))
   (divisible-by? 7  (to-num [d5 d6 d7]))
   (divisible-by? 11 (to-num [d6 d7 d8]))
   (divisible-by? 13 (to-num [d7 d8 d9]))
   (divisible-by? 17 (to-num [d8 d9 d10]))))

;; 3800ms
;; this is the simplest solution - generate all permutations (necessarily pandigital)
;; and then test for the divisibility property. 
(defn answer []
  (->> (comb/permutations (range 0 10))
       (filter special?)
       (map to-num)
       (reduce +)))

;; around 8 ms
;; could further exploit properties of numbers, but this seems more than fast enough
;; combared to the permutations solution
(defn faster-answer []
  (reduce +
          (for [d10 (range 0 10)

                d9 (range 0 10)
                :when (distinct? d9 10)
                
                d8 (range 0 10)
                :when (and (distinct? d8 d9 d10)
                           (divisible-by? 17 (to-num [d8 d9 d10])))
                
                d7 (range 0 10)
                :when (and (distinct? d7 d8 d9 d10)
                           (divisible-by? 13 (to-num [d7 d8 d9])))
                
                d6 (range 0 10)
                :when (and (distinct? d6 d7 d8 d9 d10)
                           (divisible-by? 11 (to-num [d6 d7 d8])))
                
                d5 (range 0 10)
                :when (and (distinct? d5 d6 d7 d8 d9 d10)
                           (divisible-by? 7 (to-num [d5 d6 d7])))
                
                d4 (range 0 10)
                :when (and (distinct? d4 d5 d6 d7 d8 d9 d10)
                           (divisible-by? 5 (to-num [d4 d5 d6])))
                
                d3 (range 0 10)
                :when (and (distinct? d3 d4 d5 d6 d7 d8 d9 d10)
                           (divisible-by? 3 (to-num [d3 d4 d5])))
                
                d2 (range 0 10)
                :when (and (distinct? d2 d3 d4 d5 d6 d7 d8 d9 d10)
                           (divisible-by? 2 (to-num [d2 d3 d4])))
                
                d1 (range 1 10)
                :when (distinct? d1 d2 d3 d4 d5 d6 d7 d8 d9 d10)]
            (to-num [d1 d2 d3 d4 d5 d6 d7 d8 d9 d10]))))