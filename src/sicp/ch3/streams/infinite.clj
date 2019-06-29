(ns sicp.ch3.streams.infinite
  (:require [clojure.test :refer :all]
            [sicp.ch3.streams.stream-cons :refer [cons-m car cdr take-s filter-stream] :as ss]))

(defn factorized?
  "num [num]  -> bolean"
  [x xs]
  (cond
    (empty? xs) false
    (zero? (rem x (first xs))) true
    :else (recur x (rest xs))))

(defn to-vector "Stream -> vector"
  [s]
  (if (nil? s) [] (conj (to-vector (cdr s)) (car s))))

(defn n-primes
  "num -> Stream
   primes up to n"
  [n]
  (if (<= n 3)
    (cons-m 3 (cons-m 2 nil))
    (let [factors (to-vector (n-primes (dec n)))]
      ;; (prn factors)
      (if (factorized? n factors)
        (recur (dec n))
        (cons-m n (n-primes (dec n)))))))

(defn nums [start step] (cons-m start (nums (+ start step) step)))

(defn fibgen [a b] (cons-m a (fibgen b (+ a b))))

(defn sieve "Stream -> Stream"
  [s]
  (cons-m (car s)
          (do
            ;; (prn (car s))
            (sieve (filter-stream #(not= 0 (rem % (car s)))
                                  (cdr s))))))

(def primes (sieve (nums 2 1)))

;; (def primes
;;   (cons-m 2 (cons-m 3
;;                     (filter-stream
;;                       (fn[x] (not (factorized? x (to-vector (filter-stream #(< % (dec x)) primes)))))
;;                       s-nums))))

(def ones (cons-m 1 ones))

(defn add-streams [s1 s2] (ss/map-streams + s1 s2))

(def integers (cons-m 1 (add-streams ones integers)))

(def fibs (cons-m 0 (cons-m 1 (ss/map-streams + fibs (cdr fibs)))))

(def power-of-2 (cons-m 1 (ss/map-stream #(* % 2) power-of-2)))

(def s-nums (nums 3 2))

(def prime?)

(def primes-2 (cons-m 2 (filter-stream prime? s-nums)))

(defn square [x] (* x x))

(defn prime? [x]
  (loop [ps primes-2]
    (cond
      (> (square (car ps)) x) true
      (zero? (rem x (car ps))) false
      :else (recur (cdr ps)))))

(def s11 (cons-m 1  (ss/map-streams + s11 s11)))

(def factorials (cons-m 1 (ss/map-streams * factorials integers)))

(defn partial-sums "Stream -> Stream"
  [s]
  (letfn [(iter [s acc]
            (do
              ;; (prn acc)
              (cons-m acc (iter (cdr s) (+ (car s) acc)))))]
    (cdr (iter s 0))))

(defn scale-stream "Stream -> Stream * fact"
  [xs k]
  (cons-m (*' (car xs) k) (scale-stream (cdr xs) k)))

(defn merge-streams [xs ys]
  (cond
    (< (car xs) (car ys)) (cons-m (car xs) (merge-streams (cdr xs) ys))
    (> (car xs) (car ys)) (cons-m (car ys) (merge-streams xs (cdr ys)))
    :else (cons-m (car xs) (merge-streams (cdr xs) (cdr ys)))))

(def hamming-seq (cons-m 1
                         (merge-streams (scale-stream hamming-seq 5)
                                        (merge-streams
                                         (scale-stream hamming-seq 2) (scale-stream hamming-seq 3)))))

(def factorials1 (cons-m 1 (ss/map-streams * factorials1 integers)))

(defn powerx [x]
  (letfn [(iter [acc]
            (cons-m acc (iter (* acc x))))]
    (iter 1)))

(defn exp [x] (partial-sums (ss/map-streams * (powerx x) (ss/map-stream #(/ 1 %) factorials1))))

(defn abs[x] (if (neg? x) (* -1 x) x))

(defn close-enough [a b dx] (< (abs (- a b)) dx))

(defn power-sum "Stream of coeficietns a, x -> a0 + a1*x + a2*x^2 + ..."
  [as x]
  (partial-sums (ss/map-streams * (powerx x) as)))

(defn exp1 [x] (power-sum (ss/map-stream #(/ 1 %) factorials1) x))

(defn to-stream "[x] -> Stream of x"
  [xs]
  (when (seq xs) (cons-m (first xs) (to-stream (next xs)))))

(defn cycle-m  "[x] -> Stream of x"
  [xs]
  (letfn  [(iter [ys] (if (seq ys)
                        (cons-m (first ys) (iter (next ys)))
                        (recur xs)))]
    (iter xs)))

(defn cos [x]
 (let [iter (fn iter [i elem]
              (cons ))])
 )

(deftest test-n-primes

  (testing "cycle-m"
    (is (= [1 1 1] (take-s (cycle-m [1]) 3)))
    (is (= [1 2 1 2 1] (take-s (cycle-m [1 2]) 5)))
    )

  (testing "ex 3.59"
    (is (= (range 5) (take-s (to-stream (range 6)) 5)))

    (is (= [2 4 6] (take-s (scale-stream integers 2) 3)))
    (is (= [1 1 2 6 24] (take-s factorials1 5)))
    (is (= [1 2 4 8 16] (take-s (powerx 2) 5)))
    (is (= [1 3 9] (take-s (powerx 3) 3)))
    ;; (is (= [1/2 1/4 1/16] (take-s (exp 1/2) 3)))
    (is (close-enough Math/E (ss/stream-ref (exp 1) 3) 0.1))
    (is (close-enough (Math/exp 0.2) (ss/stream-ref (exp 0.2) 3) 1e-4))
    (is (= (take-s (exp1 1) 5) (take-s (exp 1) 5)))
    (is (close-enough (Math/exp 0.2) (ss/stream-ref (exp1 0.2) 3) 1e-4)))

  (testing "ex 3.58"
    (let [expand (fn expand [numer den radix]
                   (cons-m (quot (* numer radix) den)
                           (expand (rem (* numer radix) den) den radix)))]
      (is (= [1 4 2 8 5] (take-s (expand 1 7 10) 5)))
      (is (= [3 7 5 0 0] (take-s (expand 3 8 10) 5)))))

  (testing "ex 3.56 Hamming"
    (is (= [1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36]  (take-s hamming-seq 20)))
    ;; (is (= 2125764000 (ss/stream-ref hamming-seq 1690)))
    ;; (is (= 519312780448388736089589843750000000000000000000000000000000000000000000000000000000 (ss/stream-ref hamming-seq (int (- 1e6 1)))))
    )

  (testing "ex"
    (let [ps-int (partial-sums integers)]
      (is (= [1 3 6 10 15] (take-s ps-int 5)))
      (is (= 55 (ss/stream-ref ps-int 9))))

    (is (= [1 2 4 8 16] (take-s s11 5)))
    (is (= [1 1 2 6 24 120] (take-s factorials 6))))

  (testing "primes"
    (is (= [2 3] (take-s primes-2 2)))
    (is (= [2 3 5] (take-s primes-2 3)))
    (is (= [2 3 5 7 11 13] (take-s primes-2 6)))
    (is (= 233 (ss/stream-ref primes-2 50))))

  (testing "fibs fibonacci"
    (is (= [1 2 4 8 16 32] (take-s power-of-2 6))))

  (testing "fibs fibonacci"
    (is (= [0 1 1 2 3 5 8 13 21] (take-s fibs 9))))

  (testing "implicit Stream"
    (is (= [1 1 1] (ss/take-s ones 3)))
    (is (= [2 2 2] (ss/take-s (ss/map-streams + ones ones) 3)))
    (is (= [2 2 2] (ss/take-s (add-streams ones ones) 3)))
    (is (= [1 2 3] (ss/take-s integers 3)))
    (is (= (range 1 11) (ss/take-s integers 10)))

    (let [ones ((fn ones [] (cons-m 1 (ones))))]
      (is (= [1 1 1] (ss/take-s ones 3)))
      (is (= (repeat 4 1) (ss/take-s ones 4)))))

  (testing "fibonacci"
    (let [xs (fibgen 1 1)]
      (is (= [1 1 2 3 5 8] (take-s xs 6)))))

  (testing "primes"
    (is (= [2 3] (take-s primes 2)))
    (is (= [2 3 5] (take-s primes 3)))
    (is (= [2 3 5 7] (take-s primes 4)))
    (is (= [2 3 5 7 11 13] (take-s primes 6)))
    (is (= 5 (ss/stream-ref primes 2)))
    (is (= 233 (ss/stream-ref primes 50))))

  (testing "nums"
    (let [xs (nums 1 2)]
      (is (= [1 3 5] (take-s xs 3)))))

  (testing "to-vector"
    (is (= [1] (to-vector (cons-m 1 nil))))
    (is (= [2 1] (to-vector (cons-m 1 (cons-m 2 nil))))))

  (testing "n-primes"
    (is (= [2 3] (to-vector (n-primes 3))))
    (is (= [2 3 5] (to-vector (n-primes 5))))
    (is (= [2 3 5 7 11] (to-vector (n-primes 12))))))

(test-n-primes)
