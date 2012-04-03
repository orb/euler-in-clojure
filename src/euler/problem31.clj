(ns euler.problem31)

;; In England the currency is made up of pound, £, and pence, p, and
;; there are eight coins in general circulation:

;; 1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
;; It is possible to make £2 in the following way:

;; 1£1 + 1x50p + 2x20p + 1x5p + 1x2p + 3x1p
;; How many different ways can £2 be made using any number of coins?


(def all-coins [200 100 50 20 10 5 2 1])

(defn coinperms [money coins]
  (cond

   ;; if we got to 0, we have successful terminal state
   (= money 0)
   1

   ;; if we are out of coin types, we've failed
   ;; if the logic around the final penny were smarter, this should never occur,
   ;; but this bit of laziness doesn't cause any problems
   (not (seq coins))
   0

   
   :else
   (let [next-coin (first coins)
         next-max (quot money next-coin)]
     ;; take the next highest coin and calculate the maximum number of that coin
     ;; we recurse on all permutations of 0..next-max and sum the results.
     (reduce +
             (map #(coinperms (- money (* % next-coin)) (rest coins))
                  (range 0 (inc next-max)))))))

(defn answer
  ([] (answer 200))

  ([n] (coinperms n all-coins)))