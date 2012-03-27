(ns euler.problem22
  (:require [clojure.string :as str]))

;; Using names.txt (right click and 'Save Link/Target As...'), a 46K
;; text file containing over five-thousand first names, begin by
;; sorting it into alphabetical order. Then working out the
;; alphabetical value for each name, multiply this value by its
;; alphabetical position in the list to obtain a name score.

;; For example, when the list is sorted into alphabetical order,
;; COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name
;; in the list. So, COLIN would obtain a score of 938 53 = 49714.

;; What is the total of all the name scores in the file?


;; weak parsing, but that's not the goal here
(defn load-names [filename]
  (re-seq #"[A-Z]+" (slurp filename)))

(defn score-letter [c]
  (- (int c) (dec (int \A))))

(defn score-name [name]
  (reduce + (map score-letter name)))

;; final score for name based on index position
;; problem wants 1-based, so we need to inc our
;; 0-based index
(defn score-indexed [i name]
  (* (inc i)
     (score-name name)))

(defn answer
  ([]
     (answer (load-names "src/euler/names.txt")))

  ([names]
     (->> (sort names)
          (map-indexed score-indexed)
          (reduce +))))

