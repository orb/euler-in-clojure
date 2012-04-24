(ns euler.problem36)

;;; The decimal number, 585 = 10010010012 (binary), is palindromic in
;;; both bases.

;;; Find the sum of all numbers, less than one million, which are
;;; palindromic in base 10 and base 2.

;;; (Please note that the palindromic number, in either base, may not
;;; include leading zeros.)

(defn base-b-string [b n]
  (clojure.pprint/write n :base b :stream nil))

(def base-2-string
  (partial base-b-string 2))

(def base-10-string
  (partial base-b-string 10))

(defn palindrome? [text]
  (loop [p1 0
         p2 (dec (count text))]
    (if (> p1 p2)
      true
      (if (= (get text p1)
             (get text p2))
        (recur (inc p1) (dec p2))
        false))))

(defn answer
  ([] (answer 1000000))
  ([n]
     (->> (range 1 n)
          (filter #(palindrome? (base-10-string %)))
          (filter #(palindrome? (base-2-string %)))
          (reduce +))))