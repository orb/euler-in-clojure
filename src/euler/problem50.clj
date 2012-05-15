(ns euler.problem50
  (:use [euler.problem35 :only [prime?]]))


;; map, but passing the entire subseq at a point
;; instead of the item.  ex:
;; (map-list identity [1 2 3)
;; => ([1 2 3] [2 3] [3])
;;
;; maybe better to use a sublist generator and then simply map across it?
;; (map f (sublists items))
;; where sublists looks a lot like map-lists minus the (f) invoke. 
(defn map-sublists [f items]
  (when (seq items)
    (cons (f items)
          (lazy-seq (map-lists f (rest items))))))

;; all the primes from less than n
(defn primes-less-than [n]
  (filter prime? (range 1 n)))

(defn answer
  ([]
     (answer 1000000))

  ([n]
     (let [
           ;; given list of primes, return a tuple containing
           ;; [index sum] where length is the (zero-based) indexed
           ;; of the length of the longest sequence from the beginning
           ;; of the list whose sum is prime
           prime-length
           (fn [primelist]
             (last
              (->> (map-indexed vector (reductions + primelist))
                   (take-while #(< (second %) n))
                   (filter #(prime? (second %))))))
           
           ;; given two lists, return the list with the larger first item
           max-first
           (partial max-key first)]
       
       ;; start with a sequence of all the primes less than the given number
       ;; and thread it through the workflow
       (->> (primes-less-than n)

            ;; use map-sublists to generate each sublist and map prime-length
            ;; to compute the best subsequence from that point
            (map-sublists prime-length)

            ;; reduce that using the first value, the index
            (reduce max-first)

            ;; and return the second value of that, the actual prime that was summed to
            second))))

