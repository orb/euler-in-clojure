(ns euler.problem60
  (:require [clojure.math.combinatorics :as comb])
  #_(:require [clojure.set :as set])
  (:use [euler.problem35 :only [prime?]]))

;; The primes 3, 7, 109, and 673, are quite remarkable. By taking any
;; two primes and concatenating them in any order the result will
;; always be prime. For example, taking 7 and 109, both 7109 and 1097
;; are prime. The sum of these four primes, 792, represents the lowest
;; sum for a set of four primes with this property.

;; Find the lowest sum for a set of five primes for which any two
;; primes concatenate to produce another prime.

(defn concat-num [x y]
  (Long/parseLong (str x y)))

(defn prime-pair? [x y]
  (and (prime? (concat-num x y))
       (prime? (concat-num y x))))

(defn primeset-with? [n primes]
  (every? #(prime-pair? n %) primes))

(defn expand-primes [primes n]
  (->> primes
       (filter #(primeset-with? n %))
       (map #(conj % n))))

;; 26033
(defn solve [howmany]
  (loop [n 2 primes [#{}] best-sum Integer/MAX_VALUE]
    (if (< best-sum 30000)
      best-sum
      (if (prime? n)
        (let [expanded (expand-primes primes n)
              solved (filter #(= howmany (count %)) expanded)
              solved-sums (map #(apply + %) solved)
              new-best (apply min best-sum solved-sums)]
          (println n)
          (recur (inc n) (into primes expanded) new-best))

        (recur (inc n) primes best-sum)))))

(defn guess-solve [guess]
  (let [primes
        (->> (range)
             (filter prime?)
             (take-while #(< % guess)))]
    (println "** STARTING with" (count primes) "primes")
    (for [

          p1 primes
          :let [_ (println "*" p1)]

          p2 primes
          :when (and (< p1 p2)
                     (prime-pair? p1 p2))

          p3 primes
          :when (and (< p2 p3)
                     (prime-pair? p1 p3)
                     (prime-pair? p2 p3))

          :let [_ (println "**" p1 p2 p3)]
          p4 primes
          :when (and (< p3 p4)
                     (prime-pair? p1 p4)
                     (prime-pair? p2 p4)
                     (prime-pair? p3 p4))

          :let [_ (println "***" p1 p2 p3 p4)]
          
          p5 primes
          :when (and (< p4 p5)
                     (prime-pair? p1 p5)
                     (prime-pair? p2 p5)
                     (prime-pair? p3 p5)
                     (prime-pair? p4 p5))
          ]
      [p1 p2 p3 p4 p5])))

(defn prime-pairs [guess n]
  (->> (range (inc n) guess)
       (filter prime?)
       (filter (partial prime-pair? n))))


(defn ordered-intersect [xs ys]
  (when (and (seq xs) (seq ys))
    (let [xycompare (compare (first xs) (first ys))]
      (cond
       (= xycompare 0)
       (cons (first xs) (lazy-seq (ordered-intersect (rest xs) (rest ys))))
       
       (< xycompare 0)
       (ordered-intersect (rest xs) ys)
       
       :else 
       (ordered-intersect xs (rest ys))))))

(defn pair-solve [guess]
  (let [pairs-for
        (memoize (partial prime-pairs guess))

        primes
        (->> (range)
             (filter prime?)
             (take-while #(< % guess)))]
      
    (for [p1 primes
          :let [#_(println "*" p1)
                p1-pairs (pairs-for p1)]          
          
          p2 p1-pairs
          :when (> p2 p1)
          :let [#_(println "*" p1 p2)
                p2-pairs (pairs-for p2)
                p1p2 (ordered-intersect p1-pairs p2-pairs)]
          
          p3 p1p2
          :when (> p3 p2)
          :let [_ (println "*" p1 p2 p3)
                p3-pairs (pairs-for p3)]

          p4 (ordered-intersect p3-pairs p1p2)
          :when (> p4 p3)
          :let [_ (println "*" p1 p2 p3 p4)
                p4-pairs (pairs-for p4)]

          p5 (ordered-intersect (ordered-intersect p3-pairs p4-pairs)
                                p1p2)
          :when (> p5 p4)]
      [p1 p2 p3 p4 p5])))


(defn all-pairs [guess]
  (let [pairs-for
        (memoize (partial prime-pairs guess))

        primes
        (->> (range)
             (filter prime?)
             (take-while #(< % guess)))]
      
    (for [p1 primes
          p2 (pairs-for p1)
          :when (> p2 p1)]
      [p1 p2])))
