(ns euler.problem28)

;; Starting with the number 1 and moving to the right in a clockwise
;; direction a 5 by 5 spiral is formed as follows:

;; 21 22 23 24 25
;; 20  7  8  9 10
;; 19  6  1  2 11
;; 18  5  4  3 12
;; 17 16 15 14 13

;; It can be verified that the sum of the numbers on the diagonals is 101.

;; What is the sum of the numbers on the diagonals in a 1001 by 1001
;; spiral formed in the same way?


;; a simpler math-based answer
(defn easy-answer [n]
  (let [;; an n x n matrix will have n^2 values
        max-n
        (* n n)

        ;; looking at the matrix, the values look like:
        ;; 1 3 5 7 9 13 17 21 25 ...
        ;; the differences are repeating
        ;; +2 +2 +2 +2 +4 +4 +4 +4 +6 +6 ...
        
        ;; increments encodes a sequence of these differences
        ;; repeating 4 times a sequence of increasing positive multiples of 2
        increments
        (mapcat (partial repeat 4)
                ;; or (iterate #(+ 2 %) 2)
                (range 2 Double/POSITIVE_INFINITY 2))

        ;; starting with one, we can reduce n times over the increments
        ;; to get the nth value.  since we want to collect all the partial
        ;; values, we'll use reductions for a sequences of those values
        values
        (reductions + 1 increments)]

    (->> values
         ;; take values only while the values are in the matrix
         (take-while #(<= % max-n))
         ;; and add them up
         (reduce +))))



;; for odd n only
(defn answer
  ([]
     (answer 1001))

  ([n]
     (easy-answer n)))



;; ----------------------------------------

;; alternate answer that draws the board into a matrix, just because I
;; found the idea amusing

;; initial board of :empty values
(defn init-board [n]
  (vec
   (repeat n
           (vec (repeat n :empty)))))

(defn can-play [board pos]
  (= :empty (get-in board pos)))


;; draw the spiral from the upper right corner to the middle
;; basic idea: go straight until you can't, then turn left

(defn make-spiral [n]
  (let [mid (int (Math/ceil (/ n 2)))]
    (loop [board (init-board n)
           ;; counts down, represents the next n value to be written
           counter (* n n)
           ;; initial prior position is one right of the upper right/end positions
           pos [0 n]
           ;; implement turn left by cycling through repeated left turns
           ;; encoded as [deltax deltay]
           dirs (cycle (list [-1 0] [1 0] [0 1] [0 -1]))]

      (if (= counter 0)
        board
        (let [next-pos (map + pos (first dirs))]
          (if (can-play board next-pos)
            ;; if we can move in the current direction, make the move
            (recur (assoc-in board next-pos counter)
                   (dec counter)
                   next-pos
                   dirs)
            ;; otherwise recur trying the next direction
            (recur board counter pos (next dirs))))))))

;; 
(defn spiral-in-answer [n]
  (let [board (make-spiral n)]
    (reduce +
            (for [x (range n)
                  :let [y (- n x 1)]]
              (if (= x y)
                (get-in board [x y])
                (+ (get-in board [x x]) (get-in board [x y])))))))

