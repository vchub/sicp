(ns problems.daily.factors
  (:require [clojure.test :refer :all]))

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
    (is (= [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101] (take 26 primes0)))

    )

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
  (every? #(< 5 %) [6 8])
  (factors 4)
  (def n 20)
  (def x 2)
  (map + [1 2] [1 2])
  (range 3 (inc (Math/sqrt 20)) 2))
