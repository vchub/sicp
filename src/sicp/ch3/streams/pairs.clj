(ns sicp.ch3.streams.pairs
  (:require [clojure.test :refer :all]
            [sicp.ch3.streams.stream-cons :refer [cons-m car cdr take-s stream-ref] :as ss]
            [sicp.ch3.streams.infinite :refer [close-enough partial-sums abs] :as inf]))

(def integers (cons-m 0 (ss/map-stream inc integers)))

(defn interleave-s "Stream, Stream -> Stream"
  [s1 s2]
  (if (nil? s1)
    s2
    (cons-m (car s1) (interleave-s s2 (cdr s1)))))

(defn prime? [n] (.isProbablePrime (biginteger n) 5))

(defn pairs "Stream, Stream -> Stream of [s t]"
  [s t]
  (cons-m (vector (car s) (car t))
          (interleave-s
           (ss/map-stream #(vector (car s) %) (cdr t))
           (pairs (cdr s) (cdr t)))))

(def int-pairs (pairs integers integers))
(def int-pairs-for (for [x (range) y (range (inc x))] [y x]))

(deftest test-pairs
  (testing "interleave-s, prime?"

    (is (= [[0 0] [0 1] [1 1]] (take-s int-pairs 3)))
    (is (not= (take 8 int-pairs-for) (take-s int-pairs 8)))

    (let [ps1 (ss/filter-stream (fn [[a b]] (prime? (+ a b))) int-pairs)
          ps2 (filter (fn [[a b]] (prime? (+ a b))) int-pairs-for)
          n 4]
      (is (= (set [[0 2] [1 1] [0 3] [1 2]]) (set (take-s ps1 4))))
      (is (= (set [[0 2] [1 1] [0 3] [1 2]]) (set (take 4 ps2))))
      (is (= (set (take-s ps1 n)) (set (take n ps2))))
      )

    (is (= [0 0 1 1 2 2 3] (take-s (interleave-s integers integers) 7)))
    (is (prime? 7))
    (is (not (prime? 217))))

  (testing "integers, for and mapcat"
    (is (= [0 1 2 3] (take-s integers 4)))

    (let [ps-cat (mapcat (fn [x] (map #(vector % x) (range (inc x)))) (range))]
      (is (= [[0 0] [0 1] [1 1]] (take 3 int-pairs-for)))
      (is (= [[0 0] [0 1] [1 1]] (take 3 ps-cat)))
      (is (= (take 10 int-pairs-for) (take 10 ps-cat))))))

(test-pairs)
