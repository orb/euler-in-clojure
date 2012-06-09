(ns euler.problem59
  (:require [clojure.string :as str]))

;; Each character on a computer is assigned a unique code and the
;; preferred standard is ASCII (American Standard Code for Information
;; Interchange). For example, uppercase A = 65, asterisk (*) = 42, and
;; lowercase k = 107.

;; A modern encryption method is to take a text file, convert the
;; bytes to ASCII, then XOR each byte with a given value, taken from a
;; secret key. The advantage with the XOR function is that using the
;; same encryption key on the cipher text, restores the plain text;
;; for example, 65 XOR 42 = 107, then 107 XOR 42 = 65.

;; For unbreakable encryption, the key is the same length as the plain
;; text message, and the key is made up of random bytes. The user
;; would keep the encrypted message and the encryption key in
;; different locations, and without both "halves", it is impossible to
;; decrypt the message.

;; Unfortunately, this method is impractical for most users, so the
;; modified method is to use a password as a key. If the password is
;; shorter than the message, which is likely, the key is repeated
;; cyclically throughout the message. The balance for this method is
;; using a sufficiently long password key for security, but short
;; enough to be memorable.

;; Your task has been made easy, as the encryption key consists of
;; three lower case characters. Using cipher1.txt (right click and
;; 'Save Link/Target As...'), a file containing the encrypted ASCII
;; codes, and the knowledge that the plain text must contain common
;; English words, decrypt the message and find the sum of the ASCII
;; values in the original text.



;; the lower-case characters a-z
(def lower-chars (map char (range (int \a)  (inc (int \z)))))

;; read the cipher from the input file and return
;; a sequence of ascii numbers
(defn read-cipher []
  (->> (slurp "src/euler/cipher1.txt")
       (str/trim)
       (#(str/split % #","))
       (map #(Integer/parseInt %))))

;; xor a text using a given key - a single function since we have
;; a nice symmetric cipher
(defn xor [cipher-key text]
  (map bit-xor
       (cycle cipher-key)
       text))

;; test if an ascii value is in the range we consider ideal.  defined here as
;; any character a-zA-Z and space
(defn good-ascii? [c]
  (cond
   (and (>= c (int \a))
        (<= c (int \z)))
   true

   (and (>= c (int \A))
        (<= c (int \Z)))
   true

   (= \space c)
   true

   :else
   false))

;; score a decryption - higher numbers are better.  hueristic used for scoring
;; is number of "good" ascii characters
(defn score-solution [text]
  (count (filter good-ascii? text)))

;; given a set of possible keys, return the one that yields the best score
;; as always, note that max-key is little inefficient here, calling the
;; key-function for each compare
(defn best-key [encrypted possible-keys]
  (apply max-key
         #(score-solution (xor % encrypted))
         possible-keys))

;; my first solution - computes all possible keys and choses the best one
(defn solve-slow []
  (let [encrypted
        (read-cipher)

        key-set
        (for [c1 lower-chars
              c2 lower-chars
              c3 lower-chars]
          (map int [c1 c2 c3]))

        key
        (best-key encrypted key-set)

        plaintext (xor key encrypted)]

    (apply + plaintext)))



;; split a text into n alternating sublist.  "123456" -> ((1 4) (2 5) (3 6))
;; the reverse of interleave in some sense
;; (= text (apply interleave (split-n n text)))  ;; more or less
(defn split-n [n text]
  (for [i (range n)]
    (apply concat (partition 1 n (drop i text)))))


;; since each characther of the key is independent and we aren't doing
;; any sort of ngram analysis, we can solve each character of the key
;; separately and combine
(defn solve []
  (let [;; the source cipher text
        encrypted
        (read-cipher)

        ;; split into 3 seqs - there's no library call that does this?
        parts
        (split-n 3 encrypted)

        ;; the possible keys for each part are the lower case chars a-z
        key-set
        (for [c lower-chars]
          [(int c)])

        ;; combine the results of each optimal sub-key
        key
        (apply concat
               (map #(best-key % key-set)
                    parts))

        ;; recover the plaintext, as ascii values
        plaintext
        (xor key encrypted)]

      ;; and the final result is the sum of those ascii values
      (apply + plaintext)))




