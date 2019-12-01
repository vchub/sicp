(ns problems.daily.factors
  (:require [clojure.test :refer :all]))

;; ====================
;; Given a list of integers L, find the maximum length of a sequence of consecutive numbers that can be formed using elements from L.
;; For example, given L = [5, 2, 99, 3, 4, 1, 100], return 5 as we can build a sequence [1, 2, 3, 4, 5] which has length 5.

(defn inc_sub
  "[int]->([int], [int])"
  [xs]
  ;; {:pre [(not (empty? xs))]}
  (loop [acc (take 1 xs) xs (drop 1 xs)]
    (cond
      (or (empty? xs)
          (not= 1 (- (first xs) (first acc)))) [acc xs]
      :else (recur (conj acc (first xs)) (rest xs)))))

(defn max-len'
  "[int]->int"
  [xs]
  (loop [xs xs acc 0]
    (if (empty? xs)
      acc
      (let [[s xs] (inc_sub xs)]
        (recur xs (max acc (count s)))))))

(def max-len  (comp max-len' sort))


;; ====================


(def nats (lazy-seq (cons 1 (map + nats (repeat 1)))))

(defn factors0 "int -> [int]"
  [n]
  (filter (fn [x] (zero? (mod n x)))
          (cons 2 (range 3 (inc (Math/sqrt n)) 2))))

(defn factors1 "int -> [int]"
  [n]
  (filter (fn [x] (zero? (mod n x)))
          (range 2 (inc (Math/sqrt n)))))

(defn ff [n x] ((comp not zero?) (mod n x)))

(defn notfactored [n xs]
  (letfn [(pred [x] ((comp not zero?) (mod n x)))]
    (every? pred xs)))

(def primes0
  ;; (letfn [(pos-fact [n] (take-while #(< % (Math/sqrt n)) primes0))]
  (letfn [(pos-fact [n] (take-while #(< % (/ n 2)) primes0))]
    (let [odds (iterate #(+ 2 %) 3)]
      (lazy-seq (cons 2
                      (filter #(notfactored % (pos-fact %)) odds))))))

(deftest test-factors
  (testing "inc_sub"
    (is (= [[] []] (inc_sub [])))
    (is (= [[2] [4]] (inc_sub [2 4])))
    (is (= [[4 3 2] [6]] (inc_sub [2 3 4 6])))
    (is (= 3 (max-len [2 3 4 6])))
    (is (= 0 (max-len [])))
    (is (= 5 (max-len [5, 2, 99, 3, 4, 1, 100]))))

  (testing "isProbablePrime"
    (is (not (.isProbablePrime (.subtract (.pow (biginteger 2) 51) (biginteger 1)) 5))))

  (testing "primes0"
    ;; (is (ff 4 2))
    ;; (is (ff 5 2))

    (is (notfactored 3 [2]))
    (is (notfactored 7 [2,3,5]))
    (is (not (notfactored 8 [2,3,5])))
    (is (not (notfactored 9 [2,3,5])))
    (is (notfactored 11 [2,3,5]))

    (is (= [2,3,5,7] (take 4 primes0)))
    (is (= [2,3,5,7,11,13,17,19,23,29,31] (take 11 primes0)))
    (is (= [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101] (take 26 primes0))))

  (is (= (range 1 10) (take 9 nats)))
  (testing "factors0"
    (is (= [] (factors0 3)))
    (is (= [2] (factors0 4)))
    (is (= [2,3] (factors0 6)))
    (is (= [2,3] (factors0 18)))
    (is (= [2,3,9] (factors0 (* 4 18)))))

  (testing "factors1"
    (is (= [] (factors1 3)))
    (is (= [2] (factors1 4)))
    (is (= [2,3] (factors1 6)))
    (is (= [2,3] (factors1 18)))
    (is (= [2,3,4,6] (factors1 (* 2 18))))))

(test-factors)

(comment
  (sort-by - (range 10))
  (reverse (sort-by < (range 10)))

  (let [[x y & z] [1 2 3 4]]
    [x y z])

  (let [[x y & z] [1]]
    [x y z])

  (let [[x y] [1 2 3]]
    [x y])

  (< x nil)

  (every? #(< 5 %) [6 8])
  (factors 4)
  (def n 20)
  (def x 2)
  (map + [1 2] [1 2])
  (range 3 (inc (Math/sqrt 20)) 2))


;; The h-index is a metric used to measure the impact and productivity of a scientist or researcher.
;;
;; A scientist has index h if h of their N papers have at least h citations each, and the other N - h papers have no more than h citations each. If there are multiple possible values for h, the maximum value is used.
;;
;; Given an array of natural numbers, with each value representing the number of citations of a researcher's paper, return the h-index of that researcher.
;;
;; For example, if the array was:
;;
;; [4, 0, 0, 2, 3]
;; This means the researcher has 5 papers with 4, 1, 0, 2, and 3 citations respectively. The h-index for this researcher is 2, since they have 2 papers with at least 2 citations and the remaining 3 papers have no more than 2 citations.


(defn h-index "[int]->int"
  [xs]
  (let [sum (fn [xs] (reduce + xs))
        xs (sort-by - xs)]
    (sum (for [[i,x] (map-indexed #(vector %1 %2) xs) :when (<= (inc i) x)]  1))))

(deftest test-h-index
  (is (= 0 (h-index [])))
  (is (= 0 (h-index [0])))
  (is (= 1 (h-index [0 1])))
  (is (= 1 (h-index [0 1 1])))
  (is (= 2 (h-index [4, 0, 0, 2, 3]))))

(test-h-index)

(comment
  (for [[i,x] (map-indexed #(vector %1 %2) (range 10 0 -1)) :when (<= i x)]  [i,x]))
