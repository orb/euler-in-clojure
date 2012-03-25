(ns euler.problem19
  (:require [clojure.string :as str]))

;; You are given the following information, but you may prefer to do some research for yourself.

;;1 Jan 1900 was a Monday.
;;Thirty days has September,
;;April, June and November.
;;All the rest have thirty-one,
;;Saving February alone,
;;Which has twenty-eight, rain or shine.
;;And on leap years, twenty-nine.
;;A leap year occurs on any year evenly divisible by 4, but not on a
;;century unless it is divisible by 400.
;;How many Sundays fell on the first of the month during the twentieth
;;century (1 Jan 1901 to 31 Dec 2000)?


(defn leap-year? [y]
  (cond
   (zero? (mod y 400))
   true

   (zero? (mod y 100))
   false

   (zero? (mod y 4))
   true

   :else
   false))

(defn num-days [y m]
  (cond
   (contains? #{4 6 9 11} m)
   30
   (contains? #{1 3 5 7 8 10 12} m)
   31
   :else ;; feb!
   (if (leap-year? y) 29 28)))
  

;; calculate the number of days in each month over the range
;; reduce the number of days, mod 7, to calculate day of wek
;; zero values are sundays
(defn answer []
  (let [days-in-month
        (for [year (range 1901 (inc 2000))
              month (range 1 (inc 12))]
                        (num-days year month))

        day-of-month
        (reductions #(mod (+ %1 %2) 7)
                    2 ;; tuesday
                    days-in-month)
        ;;_ (println (partition 12 day-of-month))
        
        count-days (frequencies day-of-month)
        ;;_ (println count-days)
        ]

    (get count-days 0)))
