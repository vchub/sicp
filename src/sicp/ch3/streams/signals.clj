(ns sicp.ch3.streams.signals
  (:require [clojure.test :refer :all]
            [sicp.ch3.streams.stream-cons :refer [cons-m car cdr take-s stream-ref] :as ss]
            [sicp.ch3.streams.infinite :refer [close-enough partial-sums abs] :as inf]))

(def integers (cons-m 0 (ss/map-stream inc integers)))
(def ones (cons-m 1 ones))

(defn add-streams [xs ys] (ss/map-streams + xs ys))
(defn scale-stream [k xs] (ss/map-stream #(* k %) xs))

(defn integral "Stream, num, num -> Stream"
  [xs init dt]
  (letfn [(iter [xs sum]
            (cons-m sum (iter (cdr xs) (+ sum (* (car xs) dt)))))]
    (iter xs init)))

(defn integral-0 "Stream, num, num -> Stream"
  [xs init dt]
  (def integral-0-
    (cons-m init (add-streams (scale-stream dt xs) integral-0-)))
  integral-0-)

(defn RC "num num num -> (Stream num) -> Stream"
  [R C dt]
  (fn [xs v0]
    (add-streams (scale-stream R xs)
                 (integral xs (/ 1 C) v0))))

(defn zero-cross "Stream, Stream -> Stream [0 1 -1]"
  [xs]
  (letfn [(sig [x y] (cond
                       (and (neg? x) (<= 0 y)) 1
                       (and (<= 0 x) (neg? y)) -1
                       :else 0))]
    (map sig (lazy-seq (cons (first xs) xs)) xs)))

(defn smooth "Stream -> Stream"
  [n xs]
  (letfn [(average [xs] (/ (reduce + xs) n))
          (iter [xs acc]
                (lazy-seq (cons acc (iter (next xs) (average (take n xs))))))]
    (drop 1 (iter (lazy-cat (repeat (dec n) 0) xs) 0))))

(deftest test-integral
  (testing "zero-cross"
    (let [in '(1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4)
          exp '(0 0  0  0  0 -1 0  0  0   0   1  0 0)
          in-noise '(1 -0.1 0 -1 0.1 1)
          ex-noise '(0  0   -1 0 0   1)]
      (is (= exp (take (count in) (zero-cross in))))
      (is (= [0 1/2 3/2 5/2] (take 4 (smooth 2 (range)))))
      (is (= [0 1/3 1 2 3 4] (take 6 (smooth 3 (range)))))
      (is (= ex-noise (take (count in-noise) (zero-cross (smooth 2 in-noise)))))))

  (testing "RC"
    (let [rc1 (RC 5 1 0.5)]
      (is (= 26 (stream-ref (rc1 integers 0) 5)))
      (is (= 6 (stream-ref (rc1 ones 0) 5)))))

  (testing "integral"
    (is (= (range 10) (take-s integers 10)))
    (is (= 55 (stream-ref (integral integers 0 1) 11)))
    (is (= 110 (stream-ref (integral integers 0 2) 11)))
    (is (= 10 (stream-ref (integral integers 0 1) 5)))
    (is (= 55 (stream-ref (integral-0 integers 0 1) 11)))
    (is (= (take-s (integral integers 1 0.1) 10)
           (take-s (integral-0 integers 1 0.1) 10)))
    (is (= (repeat 5 1) (take-s ones 5)))))

(test-integral)
