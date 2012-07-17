(ns euler.problem62
  (require [clojure.math.numeric-tower :as num]))
;; The cube, 41063625 (345^3), can be permuted to produce two other
;; cubes: 56623104 (384^3) and 66430125 (405^3). In fact, 41063625 is
;; the smallest cube which has exactly three permutations of its
;; digits which are also cube.

;; Find the smallest cube for which exactly five permutations of its
;; digits are cube.

(defn cube [n]
  (*' n n n))

(defn cube-root [n]
  (num/expt n (/ 1 3)))

(defn digit-order-key [n]
  (->> (str n)
       (sort)
       (apply str)))

(defn kv-has-n-values [n]
  (fn [[key values]]
    (= n (count values))))

(defn nillable-min [vals]
  (when (seq vals)
    (apply min vals)))

(defn solve-for-range [n start end]
  (->> (range start end)
       (group-by #(digit-order-key (cube %)))
       (filter (kv-has-n-values n))
       (map #(cube (apply min (second %))))
       (nillable-min)))

(defn solve-for-digits [n digits]
  (let [lower (int (num/floor (cube-root (num/expt 10 digits))))
        upper (int (num/ceil (cube-root (num/expt 10 (inc digits)))))]

    (solve-for-range n lower upper)))

(defn solve
  ([]
     (solve 5))
  
  ([n]
     (loop [digits 1]
       (let [answer (solve-for-digits n digits)]
         (if answer
           answer
           (recur (inc digits)))))))
