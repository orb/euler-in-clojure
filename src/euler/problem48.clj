(ns euler.problem48
  (require [clojure.math.numeric-tower :as num]))

;; The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.

;; Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.


(def answer-mod (num/expt 10 10))


;; I guess this would be slightly trickier without arbitrary precision?
(defn answer
  ([] (answer 1000))

  ([n]
     (mod (reduce +
                  (->> (range 1 (inc n))
                       (map #(num/expt % %))))
          answer-mod)))