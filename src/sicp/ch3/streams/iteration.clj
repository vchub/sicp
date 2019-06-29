(ns sicp.ch3.streams.iteration
  (:require [clojure.test :refer :all]
            [sicp.ch3.streams.stream-cons :refer [cons-m car cdr take-s stream-ref] :as ss]
            [sicp.ch3.streams.infinite :refer [close-enough partial-sums abs]]))

(defn square [x] (* x x))
(defn average [x y] (/ (+ x y) 2))

(defn sqrt-improve "num, num -> num"
  [guess x]
  (average guess (/ x guess)))

(defn sqrt-iter [x dx]
  (loop [guess 1]
    (if (close-enough (square guess) x dx)
      guess
      (recur (sqrt-improve guess x)))))

(defn sqrt-stream "num -> Stream"
  [x]
  (letfn [(iter [guess]
            (cons-m guess (iter (sqrt-improve guess x))))]
    (iter 1)))

(defn sqrt-from-stream [x dx]
  (car (ss/filter-stream #(close-enough (square %) x dx) (sqrt-stream x))))

(defn sqrt-guesses [x]
  (cons-m 1 (ss/map-stream #(do (prn %) (sqrt-improve % x)) (sqrt-guesses x)))
  )

(defn sqrt-guesses-1 [x]
  (def guesses (cons-m 1 (ss/map-stream #(do (prn %) (sqrt-improve % x)) guesses)))
  guesses
  )

(defn sqrt-neighbor [x dx]
  (loop [s (sqrt-stream x)]
    (let [a (car s) b (car (cdr s))]
      (if (< (abs (- a b)) dx)
        b
        (recur (cdr s)))))
  )

(deftest test-iteration
  (testing "sqrt"
    (let [dx 1e-3]
      (is (close-enough (Math/sqrt 2) (sqrt-iter 2 dx) dx))
      (is (= [1 3/2] (take-s (sqrt-stream 2) 2)))

      (is (close-enough (Math/sqrt 2) 577/408 dx))
      (is (close-enough (Math/sqrt 2) (sqrt-from-stream 2 dx) dx))
      (is (close-enough (Math/sqrt 5) (sqrt-from-stream 5 dx) dx))

      ;; (is (= [1 3/2] (take-s (sqrt-guesses 2) 4)))
      ;; (is (= [1 3/2] (take-s (sqrt-guesses-1 2) 4)))
      ;; (prn (take-s guesses 4))
      ;; (is (= [1 3/2] (take-s (sqrt-guesses-1 3) 4)))
      ;; (prn (take-s guesses 4))

      (is (close-enough (Math/sqrt 5) (sqrt-neighbor 5 dx) dx))
      )
    ))

(test-iteration)
