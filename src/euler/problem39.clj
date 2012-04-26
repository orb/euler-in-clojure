(ns euler.problem39)

;; If p is the perimeter of a right angle triangle with integral
;; length sides, {a,b,c}, there are exactly three solutions for p =
;; 120.

;; {20,48,52}, {24,45,51}, {30,40,50}

;; For which value of p  1000, is the number of solutions
;; maximised?


(defn right-triangle? [a b c]
  "test if a triangle is a right triangle"
  (= (* c c) (+ (* a a) (* b b))))

;; just check all the combinations of a b for a c that matches the solutions
;; bounds checks are probably not as tight as they could be
(defn solutions [n]
  "compute the right triangles that are a solitution for a give n"
  (let [bound (inc (quot n 2))] 
   (for [a (range 1 bound)
         b (range a bound)
         :let [c (- n (+ a b))]
         :when (right-triangle? a b c)]
         [a b c])))

(defn ansewr
  ([] (answer 1000))

  ([n]
     (apply max-key
            ;; memoize since calling solutions is not free
            ;; and max-key will make duplicate calls 
            (memoize #(count (solutions %)))            
            (range (inc n)))))