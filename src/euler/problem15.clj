(ns euler.problem15)

;; Starting in the top left corner of a 22 grid, there are 6 routes
;; (without backtracking) to the bottom right corner.

;; How many routes are there through a 2020 grid?

;; all zeros
(defn init-counts [n]
  (vec (repeat n (vec (repeat n 0)))))

(defn compute-route [counts [x y :as pos]]
  (assoc-in counts pos
            (cond
             (or (zero? y) (zero? x))
             1

             :else
             (+ (get-in counts [(dec x) y])
                (get-in counts [x (dec y)])))))

;; use a simple dynamic programming solution
(defn routes [n]
  (let [counts (init-counts (inc n))
        positions (for [x (range (inc n))
                        y (range (inc n))]
                    [x y])]

    (reduce compute-route counts positions)))


(defn answer
  ([] (answer 20))
  
  ([n]
     (last (last (routes n)))))



