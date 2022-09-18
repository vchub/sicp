(ns problems.daily.nu60
  (:require [clojure.test :refer :all]))

; A regular number in mathematics is defined as one which evenly divides some
; power of 60. Equivalently, we can say that a regular number is one whose only
; prime divisors are 2, 3, and 5.

; These numbers have had many applications, from helping ancient Babylonians
; keep time to tuning instruments according to the diatonic scale.

; Given an integer N, write a program that returns, in order, the first N
; regular numbers.

(defn pred [n]
  (some #(zero? (mod n %)) [2 3 5]))

(defn br-force [n]
  (take n (filter pred (range 2 (* n 2 3 5)))))

(testing "br-force"
  (is (= [2 3 4 5 6 8 9 10 12 14] (br-force 10))))

(testing "pred"
  (is (pred 2))
  (is (pred 3))
  (is (pred 5))
  (is (pred (* 2 3 7)))
  (is (not ((* 11 7) pred))))
