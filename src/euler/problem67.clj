(ns euler.problem67
  (:require [clojure.string :as str]))

;;By starting at the top of the triangle below and moving to adjacent
;;numbers on the row below, the maximum total from top to bottom is
;;23.

;;3
;;7 4
;;2 4 6
;;8 5 9 3

;;That is, 3 + 7 + 4 + 9 = 23.

;;Find the maximum total from top to bottom in triangle.txt (right
;;click and 'Save Link/Target As...'), a 15K text file containing a
;;triangle with one-hundred rows.

;;NOTE: This is a much more difficult version of Problem 18. It is not
;;possible to try every route to solve this problem, as there are 299
;;altogether! If you could check one trillion (1012) routes every
;;second it would take over twenty billion years to check them
;;all. There is an efficient algorithm to solve it. ;o)



;; read a line of space separated numbers to a vector of ints
(defn read-ints [line]
  (vec (map #(Integer/parseInt %)
        (str/split line #"\s"))))

;; read a file of lines of number into a vector of vectors
(defn load-triangle [filename]
  (let [lines (str/split (slurp filename) #"\r?\n")]
    (vec (map read-ints lines))))


;; use a different technique from problem 18, just for kicks.
;; in this case, we compute the best paths from the prior line
;; summing over the pairs, concatting in the first and last elements, which
;; have no pairs. could also extend the original array and do a simpler concat

;; this technique doesn't appear to be faster, but the code may be a little easier
;; to follow
(defn best-path-from [line]
  (vec
   (concat [(first line)]
           (map #(apply max %)
                (partition 2 1 line))
           [(last line)])))

;; new maximal cost of the node is the sum of the best (max) of the
;; prior line and the current value
(defn triangle-reduce [line1 line2]
  (map + (best-path-from line1) line2))


(defn answer
  ([]
     (answer (load-triangle "src/euler/triangle.txt")))

  ([triangle]
     (->> triangle
          (reduce triangle-reduce)
          (reduce max))))



