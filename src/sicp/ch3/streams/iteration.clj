(ns sicp.ch3.streams.iteration
  (:require [clojure.test :refer :all]
            [sicp.ch3.streams.stream-cons :refer [cons-m car cdr take-s stream-ref] :as ss]
            [sicp.ch3.streams.infinite :refer [close-enough partial-sums abs] :as inf]))

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
  (cons-m 1 (ss/map-stream #(do (prn %) (sqrt-improve % x)) (sqrt-guesses x))))

(defn sqrt-guesses-1 [x]
  (def guesses (cons-m 1 (ss/map-stream #(do (prn %) (sqrt-improve % x)) guesses)))
  guesses)

(defn sqrt-neighbor [x dx]
  (loop [s (sqrt-stream x)]
    (let [a (car s) b (car (cdr s))]
      (if (< (abs (- a b)) dx)
        b
        (recur (cdr s))))))

(def pi-stream
  (letfn [(iter [n]
            ;; (prn n)
            (cons-m (/ 1.0 n) (ss/map-stream - (iter (+ n 2)))))]
    (inf/partial-sums (inf/scale-stream (iter 1) 4))))

(defn euler-transform "Stream -> Stream"
  [s]
  (let [s0 (stream-ref s 0)
        s1 (stream-ref s 1)
        s2 (stream-ref s 2)]
    (cons-m
     (- s2 (/ (square (- s2 s1))
              (+ s0 (* -2 s1) s2)))
     (euler-transform (cdr s)))))

(defn make-tableau "transform, Stream -> Stream of Streams"
  [transform s]
  (cons-m s (make-tableau transform (transform s))))

(defn accelerated-seq
  [transform s]
  (ss/map-stream car (make-tableau transform s)))

(def ln2-stream
  (letfn [(iter [n]
            ;; (prn n)
            (cons-m (/ 1.0 n) (ss/map-stream - (iter (+ n 1)))))]
    (inf/partial-sums (iter 1))))

(defn log2 [x] (/ (Math/log x) (Math/log 2)))

(deftest test-iteration
  (testing "ln2"
    (is (= 1.0 (log2 2)))
    (is (= 2.0 (log2 4)))
      ;; (is (= [4] (take-s ln2-stream 8)))
    (is (not (close-enough (Math/log 2) (ss/stream-ref ln2-stream 48) 0.01)))
    (is (close-enough (Math/log 2) (ss/stream-ref (euler-transform ln2-stream) 1) 1e-2))
    (is (close-enough (Math/log 2) (ss/stream-ref (accelerated-seq euler-transform ln2-stream) 6) 1e-10))

    )
  (testing "pi"
      ;; (is (= [4 3/2] (take-s pi-stream 14)))
    (is (close-enough Math/PI (ss/stream-ref pi-stream 99) 0.01))
    ;; (is (= [4] (take-s (euler-transform pi-stream) 8)))
    (is (close-enough Math/PI (ss/stream-ref (euler-transform pi-stream) 8) 1e-3))
    ;; (is (= [4] (take-s (accelerated-seq euler-transform pi-stream) 8)))
    (is (close-enough Math/PI (ss/stream-ref (accelerated-seq euler-transform pi-stream) 8) 1e-14)))

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

      (is (close-enough (Math/sqrt 5) (sqrt-neighbor 5 dx) dx)))))

(test-iteration)
