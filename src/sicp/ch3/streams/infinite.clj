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
  (if (nil? s) [] (conj (to-vector (cdr s)) (car s)))
  )

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
        (cons-m n (n-primes (dec n)))
        ))))

(defn nums [start step] (cons-m start (nums (+ start step) step)))
(def s-nums (nums 5 2))

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

(deftest test-n-primes
  (testing "implicit Stream"
    (let [ones ((fn ones [] (cons-m 1 (ones))))]
    (is (= [1 1 1] (ss/take-s ones 3)))
    (is (= (repeat 4 1) (ss/take-s ones 4)))
    ))

  (testing "fibonacci"
    (let [xs (fibgen 1 1)]
    (is (= [1 1 2 3 5 8] (take-s xs 6))))
    )

  (testing "primes"
    (is (= [2 3] (take-s primes 2)))
    (is (= [2 3 5] (take-s primes 3)))
    (is (= [2 3 5 7] (take-s primes 4)))
    (is (= [2 3 5 7 11 13] (take-s primes 6)))
    (is (= 5 (ss/stream-ref primes 2)))
    (is (= 233 (ss/stream-ref primes 50)))
    )

  (testing "nums"
    (let [xs (nums 1 2)]
    (is (= [1 3 5] (take-s xs 3))))
    )

  (testing "to-vector"
    (is (= [ 1] (to-vector (cons-m 1 nil))))
    (is (= [ 2 1 ] (to-vector (cons-m 1 (cons-m 2 nil)))))
    )

  (testing "n-primes"
    (is (= [2 3] (to-vector (n-primes 3))))
    (is (= [2 3 5] (to-vector (n-primes 5))))
    (is (= [2 3 5 7 11] (to-vector (n-primes 12))))
    )
  )

(test-n-primes)
