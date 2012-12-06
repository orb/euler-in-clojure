(ns euler.problem68
  (:require [clojure.set :as sets]))


(def set10 (set (range 1 11)))

(defn range-excluding [& nums]
  (sets/difference set10 (set nums)))

(defn solution-to-str [nums]
  (apply str nums))

(defn length-is-16 [numstr]
  (= 16 (count numstr)))



;; quick and dirty solution.  the for loop generates all possible solutions,
;; pruning impossible solutions early, checking uniquenes, group sums and the condition
;; that the first outer node be the smallest.
;;
;; the variables a b c d e represent the 5 outer nodes and xab (for example) represents
;; the note connected to both a and b
(defn solve []
  (let [potential-solutions
        (for [a   (range-excluding)
              xae (range-excluding a)
              xab (range-excluding a xae)
              :let [group_a (+ a xae xab)]
              
              b  (range-excluding a xae xab)
              :when (> b a)
              xbc (range-excluding a b xae xab)
              :let [group_b (+ b xab xbc)]
              :when (= group_a group_b)

              c (range-excluding a b xae xab xbc)
              :when (> c a)
              xcd (range-excluding a b c xae xab xbc)
              :let [group_c (+ c xbc xcd)]
              :when (= group_c group_a)

              d (range-excluding a b c xae xab xbc xcd)
              :when (> d a)
              xde (range-excluding a b c d xae xab xbc xcd)
              :let [group_d (+ d xcd xde)]
              :when (= group_d group_a)

              e (range-excluding a b c d xae xab xbc xcd xde)
              :when (> e a)
              :let [group_e (+ e xae xde)]
              :when (= group_e group_a)]

          [a xae xab b xab xbc c xbc xcd d xcd xde e xde xae])]

    ;; for each solution array: process, sort and select the largest
    ;; one leaves the solution as a string since it's not really
    ;; relevant to the problem
    (->> potential-solutions
         (map solution-to-str)
         (filter length-is-16)
         (sort)
         (last))))