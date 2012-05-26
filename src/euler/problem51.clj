(ns euler.problem51
  (:use [euler.problem35 :only [prime?]])
  (:require [clojure.math.combinatorics :as comb]))



;; By replacing the 1st digit of *3, it turns out that six of the nine
;; possible values: 13, 23, 43, 53, 73, and 83, are all prime.

;; By replacing the 3rd and 4th digits of 56**3 with the same digit,
;; this 5-digit number is the first example having seven primes among
;; the ten generated numbers, yielding the family: 56003, 56113,
;; 56333, 56443, 56663, 56773, and 56993. Consequently 56003, being
;; the first member of this family, is the smallest prime with this
;; property.

;; Find the smallest prime which, by replacing part of the number (not
;; necessarily adjacent digits) with the same digit, is part of an
;; eight prime value family.


;; a lazy sequence of primes
(def primeseq
  (filter prime?
          (drop 2 (range)))) ;; start at 2


;; replace the character at a given position with another char
(defn replace-char-at-pos [text char pos]
  (str (subs text 0 pos)
       char
       (subs text (inc pos))))

;; put stars at the given positions in a string
(defn sub-stars-at [text positions]
  (reduce #(replace-char-at-pos %1 "*" %2)
          text
          positions))

;; find all the star permutations in a given string for a given character
(defn star-perms-char [text dchar]
    (let [matching (keep-indexed #(when (= %2 dchar) %1) text)
          update-sets (keep seq (comb/subsets matching))]
      (when (seq update-sets)
        (map (partial sub-stars-at text) update-sets))))

;; find all the star permutations in string for a set of characters
;; the parameter order here is a little different because I was playing with
;; partial evaluation
(defn star-perms [chars text]
    (mapcat (partial star-perms-char (str text)) chars))

;; this is the special case function where we want to find all
;; star permutations for a numeric string
(defn all-star-perms [text]
  (star-perms "01234556789" text))

;; this first version of the solution is a little slow, so I re-wrote
;; the final version.  However, I found the technique here
;; interesting, so I decided to keep the code around.

;; note - the solution returns only the pattern, in this case "*2*3*3"
(defn slow-solve []
  (loop [[prime & primes] primeseq
         acc {}]
    (let [new-perms
          (zipmap (all-star-perms (str num))
                  (repeat 1))

          new-acc
          (merge-with + acc new-perms)
          
          solution
          (filter #(>= (new-acc %) 8)
                  new-perms)]

      (if (seq solution)
        (first solution) 
        (recur primes new-acc)))))




;; the final solution is below.  It uses the star-perms and primeseq
;; from above, otherwise it's complete


;; given a star-num, return all the prime numbers it generates
(defn star-primes [star-num]
  (->> "0123456789"
       (map #(clojure.string/replace star-num #"\*" (str %)))
       (remove #(= \0 (nth % 0)))
       (map #(Long/parseLong %))
       (filter prime?)))

;; given a star-num, is it n-prime?  (does it generate n primes?)
(defn n-prime-pattern? [n star-num]
  (>= (count (star-primes star-num)) n))


;; does a prime number generate n primes? - this test is optimized to only
;; guarantee returning true for the smallest of the numbers in the n-prime
;; group.  
(defn n-prime? [n prime]
  ;; test if any of the star-permutations from a prime are n-prime
  ;; only generates star pattens for the minimimal set of numbers
  ;; needed to be sure to generate the smallest number  
  (some #(n-prime-pattern? n %)
        (star-perms (subs "0123456789" 0 (- 11 n)) prime)))


;; This solution is a little slower than some of the others.  Like
;; most of my solutions, it can return a solution for any n value, not
;; just the 8 value.  That means I can't take advantage of a few of
;; the optimizations that other solutions did specifically for n=8.
;; On the other hand, some of the helper functions, like n-prime? are
;; optimized for the problem of finding the first n-prime in a family.
;; However, since the solution here is more than efficient enough, I'll
;; go with it 
(defn solve
  ([]
     (solve 8))

  ([n]
     (->> ;; start with the primes sequence
          primeseq

          ;; filter the n-primes for the given n
          (filter (partial n-prime? n))

          ;; and take the first one
          first)))



