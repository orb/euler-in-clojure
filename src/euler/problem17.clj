(ns euler.problem17)

;;If the numbers 1 to 5 are written out in words: one, two, three,
;;four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in
;;total.

;If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?


;;; NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage.

(def lookup
  {1 "one"
   2 "two"
   3 "three"
   4 "four"
   5 "five"
   6 "six"
   7 "seven"
   8 "eight"
   9 "nine"
   10 "ten"
   11 "eleven"
   12 "twelve"
   13 "thirteen"
   14 "fourteen"
   15 "fifteen"
   16 "sixteen"
   17 "seventeen"
   18 "eighteen"
   19 "nineteen"
   20 "twenty"
   30 "thirty"
   40 "forty"
   50 "fifty"
   60 "sixty"
   70 "seventy"
   80 "eighty"
   90 "ninety"
   100 "hundred"
   1000 "thousand"})


(defn and-word [w1 w2]
  (if w2
    (str w1 "and" w2)
    w1))

(defn number-word
  ([n]
     (cond
      (zero? n)
      nil
      
      (< n 20)
      (lookup n)

      (< n 100)
      (str (lookup (* 10 (quot n 10))) (number-word (rem n 10)))

      (< n 1000)
      (and-word (str (number-word (quot n 100))
                     (lookup 100))
                (number-word (rem n 100)))

      :else
      (str (number-word (quot n 1000))
           (lookup 1000)
           (number-word (rem n 1000))))))


(defn answer
  ([] (answer 1000))
  ([n] (reduce +
               (map #(count (number-word %))
                    (range 1 (inc n))))))



