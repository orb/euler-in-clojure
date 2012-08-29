(ns euler.test.core
  (:use [euler.core])
  (:use [clojure.test]))

(deftest sanity
  (is true "a test exists"))

(deftest will-fail
  (is false true))