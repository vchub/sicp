(ns sicp.ch2.cons (:require [clojure.test :refer :all]))

;; (defn cons
;;   [x y]
;;   (letfn [(dispatch [m]
;;             (condp = m
;;               0 x
;;               1 y
;;               (throw (Exception. (str "wrong selector " m)))))]
;;     dispatch))

(defn cons-0
  [x y]
  (fn [m] (condp = m
            0 x
            1 y
            (throw (Exception. (str "wrong selector " m))))))

(defn car [c] (c 0))
(defn cdr [c] (c 1))

(testing
 (let [x (cons-0 1 2)]
   (is (= 1 (car x)))
   (is (= 2 (cdr x)))
   (is (thrown? Exception (x 2)))
   (is (thrown-with-msg? Exception #"wrong selector 2" (x 2)))))

(defn cons-1 [x y] (fn [m] (m x y)))
(defn car-1 [c] (c (fn [x y] x)))
(defn cdr-1 [c] (c (fn [x y] y)))

(testing
 (let [x (cons-1 1 2)]
   (is (= 1 (car-1 x)))
   (is (= 2 (cdr-1 x)))))

(defn cons-int [x y] (* (Math/pow 2 x) (Math/pow 3 y)))

(defn recip-pow [x base]
  (loop [x x cnt 0]
    (cond
      (zero? (mod x base)) (recur (/ x base) (inc cnt))
      :else cnt)))

(defn car-int [c] (recip-pow c 2))
(defn cdr-int [c] (recip-pow c 3))

(testing
 (let [x (cons-int 1 2)]
   (is (= 1 (car-int x)))
   (is (= 2 (cdr-int x))))
  (is (= 3 (car-int (cons-int 3 5))))
  (is (= 5 (cdr-int (cons-int 3 5)))))
