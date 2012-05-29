(ns euler.problem54
  (:require [clojure.string :as str]))

;; abbreviating problem description

;; In the card game poker, a hand consists of five cards and are
;; ranked, from lowest to highest, in the following way:

;; High Card: Highest value card.
;; One Pair: Two cards of the same value.
;; Two Pairs: Two different pairs.
;; Three of a Kind: Three cards of the same value.
;; Straight: All cards are consecutive values.
;; Flush: All cards of the same suit.
;; Full House: Three of a kind and a pair.
;; Four of a Kind: Four cards of the same value.
;; Straight Flush: All cards are consecutive values of same suit.
;; Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.


;; The file, poker.txt, contains one-thousand random hands dealt to
;; two players. Each line of the file contains ten cards (separated by
;; a single space): the first five are Player 1's cards and the last
;; five are Player 2's cards. You can assume that all hands are valid
;; (no invalid characters or repeated cards), each player's hand is in
;; no specific order, and in each hand there is a clear winner.

;; How many hands does Player 1 win?

;; convert a character rank to a numeric value. started at 2 to make the results
;; easier to follow
(defn rank-value [rank]
  ({\2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9 \T 10 \J 11 \Q 12 \K 13 \A 14}
   rank))

;; sort and return the card ranks (by value) from high to lo
(defn sort-ranks [[[r1 s1] [r2 s2] [r3 s3] [r4 s4] [r5 s5]]]
  (reverse (sort (map rank-value [r1 r2 r3 r4 r5]))))

;; is there a flush in the hand?
(defn flush? [[[c1 s1] [c2 s2] [c3 s3] [c4 s4] [c5 s5]]] 
  (= s1 s2 s3 s4 s5))

;; if the hand contains a straight, return the rank of the highest card
(defn straight-card [[[r1 s1] [r2 s2] [r3 s3] [r4 s4] [r5 s5] :as hand]] 
  (let [ranks (sort-ranks hand)]
    (if (every? (fn [[x y]] (= 1 (- x y)))
                (partition 2 1 ranks))
      (first ranks)
      nil)))

;; return a reverse map of card frequencies:
;; count -> vector of rank values
;; ex:
;; {2 [3 4], 1 [14]} would be pair of 3s, pair of 4s, A
;; {4: [10], 1 [5]} - four tens and a 5
(defn card-freqs [[[r1 s1] [r2 s2] [r3 s3] [r4 s4] [r5 s5]]]
  (let [counts (frequencies (map rank-value [r1 r2 r3 r4 r5]))]
    (reduce #(merge-with into %1 %2) 
            (map (fn [[k v]] {v [k]})  counts))))


;; finally, produce a hand score.  rather than try and convert the hand
;; to a numeric value, return an vector of values.  scores can be compared
;; from most significant value down.  The first value indicates the value of
;; the type of hand, and additional values represent kickers/tie-breaker values
;; for deciding the winner between similar hands
;;
;; ex: [3 1 4] > [2 9]  > [1 1 1 1]
;;     [3 1 4] < [3 1 3] < [4] < [4 1]
;; etc... 
(defn score [hand]
  (let [straight (straight-card hand)
        flush (flush? hand)
        freqs (card-freqs hand)]

    (cond

     (and straight flush)
     [9 straight]

     (contains? freqs 4)
     [8 (first (freqs 4))]

     (and (contains? freqs 3) (contains? freqs 2))
     [7 (first (freqs 3)) (first (freqs 1))]

     flush
     (apply vector 6 (sort-ranks hand))

     straight   
     [5 straight]

     (contains? freqs 3)
     [4 (first (freqs 3))]

     (= 2 (count (freqs 2)))
     (vec (flatten [3 (reverse (sort (freqs 2))) (freqs 1)]))

     (contains? freqs 2)
     (vec (flatten [2 (freqs 2) (reverse (sort (freqs 1)))]))

     :else
     (apply vector 1 (sort-ranks hand)))))

;; does hand1 beat hand2, according to comparison suggested above
(defn beats [hand1 hand2]
  (loop [[s1 & score1] (score hand1)
         [s2 & score2] (score hand2)]
    
    (cond     
     (nil? s2)
     (not (nil? s1))

     (nil? s1)
     false

     (> s1 s2)
     true

     (< s1 s2)
     false

     :else
     (recur score1 score2))))


;; parse a line of 10 cards into 2 hands
(defn parse-hand [line]
  (let [cards (str/split line #" ")]
    (partition 5 cards)))

;; read the contents a hands file
(defn read-hands [filename]
  (let [lines (str/split (slurp filename) #"\r\n")]
    (map parse-hand lines)))


;; Produce the final solution.  I didn't thoroughly test the hand evaluator.
;; I'm hoping the input file is reasonably exhaustive, but as I fixed at least
;; one problem from manual testing that wasn't caught by the input file, I'm
;; I don't think it is.
(defn solve
  ([] (solve "src/euler/poker.txt"))

  ([filename]
     (->> ;; read the hands
          (read-hands filename)
          ;; select only hands where player 1 is the winner
          (filter (fn [[hand1 hand2]] (beats hand1 hand2)))
          ;; and count 'em up
          (count))))
