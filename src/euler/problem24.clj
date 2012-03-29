(ns euler.problem24
  (:use [euler.problem20 :only [factorial]]))

;; A permutation is an ordered arrangement of objects. For example,
;; 3124 is one possible permutation of the digits 1, 2, 3 and 4. If
;; all of the permutations are listed numerically or alphabetically,
;; we call it lexicographic order. The lexicographic permutations of
;; 0, 1 and 2 are:

;; 012   021   102   120   201   210

;; What is the millionth lexicographic permutation of the digits 0, 1,
;; 2, 3, 4, 5, 6, 7, 8 and 9?


(defn countperms [items]
  (factorial (count items)))

(defn answer
  ([]
     (answer (dec 1000000)  (range 10)))

  ;; n is zero-based
  ([n items]
     (when (seq items)      
       (let [;; number of permutations of the remaining items
             num-perms (factorial (dec (count items)))
             
             ;; determine the item that would ultimately
             ;; be selected
             selected-index (quot n num-perms)
             selected-item (nth items selected-index)

             ;; remove the selected item and reduce n appropriately
             next-n (rem n num-perms)
             next-items (filter #(not= selected-item %) items)]         
         (str selected-item (answer next-n next-items))))))

