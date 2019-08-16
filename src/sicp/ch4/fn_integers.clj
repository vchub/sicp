(ns sicp.ch4.fn-integers
  (:require [clojure.test :refer :all]
            ;; [clojure.math.combinatorics :as comb]
            ))

(def zero (fn [] true))
(defn iszero? [n] (= true (n)))
(defn add-one [n] (fn [] n))
(defn sub-one [n] (if (iszero? n) zero (n)))
(defn fn->int [n] (loop [n n acc 0]
                    (if (iszero? n) acc (recur (sub-one n) (inc acc)))))

(defn add [a b] (if (iszero? b)
                  a
                  (recur (add-one a) (sub-one b))))


;; https://brilliant.org/wiki/integer-equations-star-and-bars/#
;; How many ordered sets of positive integers (a_1, a_2, a_3, a_4, a_5, a_6)
;; are there such that a_i >= i and (sum a_i)<= 100


(def tuples-num-rec
  (memoize (fn [i s n]
             (cond
               (> i n) 1
               (> i s) 0
               :else
               (reduce + (map #(tuples-num-rec (inc i) (- s %) n) (range i (inc s))))))))

(defn tuples-num-mem "num, num -> num"
  [n, s]
  (tuples-num-rec 1 s n))

(defn tuples-num "num, num -> num"
  [n, s]
  (letfn [(f [i s]
              ;; (prn i s acc)
            (cond
              (> i n) 1
              (> i s) 0
              :else
              (reduce + (map #(f (inc i) (- s %)) (range i (inc s))))))]

    (f 1 s)))

(defn tuples-seq "num, num -> [[num]]"
  [n, s]
  (letfn [(f [i s acc]
              ;; (prn i s acc)
            (cond
              (> i n) [acc]
              (> i s) []
              :else
              (mapcat #(f (inc i) (- s %) (conj acc %)) (range i (inc s)))))]

    (f 1 s [])))

(defn factorial [n]
  (loop [n n acc (biginteger 1)]
    (if (<= n 0) acc (recur (dec n) (* n acc)))))

(defn choose [n k]
  (/ (factorial n) (factorial k) (factorial (- n k))))

(deftest test-solution
  (is (= (choose (+ 79 7 -1) 6) (tuples-num-mem 6 100))))

;; (test-solution)

(deftest test-ints
  (testing "solution, big numbers"
    (is (= 1 (factorial 0)))
    (is (= 6 (factorial 3)))
    (is (= 3 (choose 3 1)))
    (is (= 1 (choose 4 0)))
    (is (= 6 (choose 4 2)))
    ;; (is (= 6 (choose 105 5)))
    ;; (is (= 6 (tuples-num 6 50)))
    )

  (testing "number of tuples"
    (is (= '() (tuples-seq 2 2)))
    (is (= '([1]) (tuples-seq 1 1)))
    (is (= '([1] [2] [3]) (tuples-seq 1 3)))
    (is (= '([1 2]) (tuples-seq 2 3)))
    (is (= '([1 2] [1 3] [2 2]) (tuples-seq 2 4)))
    (is (= '([1 2] [1 3] [1 4] [2 2] [2 3] [3 2]) (tuples-seq 2 5)))
    (is (= '([1 2 3]) (tuples-seq 3 6)))

    (is (= 1 (tuples-num 1 1)))
    (is (= 2 (tuples-num 1 2)))
    (is (= 3 (tuples-num 1 3)))
    (is (= 0 (tuples-num 2 2)))
    (is (= 1 (tuples-num 2 3)))
    (is (= 3 (tuples-num 2 4)))
    (is (= 6 (tuples-num 2 5)))
    (is (= 6 (tuples-num-mem 2 5))))

  (testing "ints basics"
    (let [z zero
          x (add-one z)
          y (add x x)]
      (is (iszero? z))
      (is (not (iszero? x)))
      (is (iszero? (sub-one z)))
      (is (= 0 (fn->int zero)))
      (is (= 1 (fn->int x)))
      (is (= 2 (fn->int y)))
      (is (= 3 (fn->int (add y x)))))))

(test-ints)

(comment
  (= 1 2 1)
  (= 1 1 1))
