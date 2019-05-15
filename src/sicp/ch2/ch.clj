(ns sicp.ch2.ch
  (:require [clojure.test :refer :all]))

(defn gcd [x y]
  (loop [a (max x y) b (min x y)]
    (if (zero? b) a (recur b (mod a b)))))

(defn abs [x] (if (neg? x) (* -1 x) x))

(defprotocol Arithmetic
  (make [x])
  (neg [x])
  (reciprical [x])
  (add [x y])
  (mult [x y])
  (sub [x y])
  (div [x y]))

(defrecord Rational [numer denom]
  Arithmetic
  (make [x]
    (let [a (abs (:numer x))
          b (abs (:denom x))
          r (gcd a b)]
      (cond
        (neg? (* (:numer x) (:denom x))) (Rational. (* -1 (/ a r)) (/ b r))
        :else (Rational. (/ a r) (/ b r)))))

  (reciprical [x] (Rational. (:denom x) (:numer x)))
  (neg [x] (Rational.  (* -1 (:numer x)) (:denom x)))

  (add [x y] (make (Rational. (+ (* (:numer x) (:denom y)) (* (:numer y) (:denom x)))
                              (* (:denom y)  (:denom x)))))
  (mult [x y] (make (Rational.  (* (:numer x)  (:numer y)) (* (:denom y)  (:denom x)))))
  (sub [x y] (add x (neg y)))
  (div [x y] (mult x (reciprical y)))

  )

(defmulti make-it (fn [a b] (map class [a b])))
(defmethod make-it [Long Long] [a b] (make (Rational. a b)))

(defn make-rat
  ([a] (list 1 1))
  ([a b] (let [r (gcd a b)]
           (list (/ a r) (/ b r)))))

(defn add-rat [x y] (make-rat (+ (*
                                  (first x) (second y))
                                 (* (first y) (second x)))
                              (* (second y)  (second x))))

(defn mul-rat [x y] (make-rat (* (first x) (first y))
                              (* (second x) (second y))))

(testing
 (is (= 1 (gcd 2 1)))
  (is (= 1 (gcd 1 2)))
  (is (= 2 (gcd 2 2)))

  (is (= (Rational. 1 1) (make-it 1 1)))
  (is (= (Rational. 3 5) (make-it 6 10)))
  (is (= (Rational. -3 5) (make-it 6 -10)))

  (is (= (Rational. 1 1) (make (Rational. 1 1))))
  (is (= (Rational. 1 1) (make (Rational. 2 2))))
  (is (= (Rational. 3 5) (make (Rational. 9 15))))

  (is (= (Rational. 4 5) (add (Rational. 1 5) (Rational. 9 15))))
  (is (= (Rational. -2 5) (sub (Rational. 1 5) (Rational. 9 15))))

  (is (= (Rational. 6 25) (mult (Rational. 2 5) (Rational. 9 15))))
  (is (= (Rational. 2 3) (div (Rational. 2 5) (Rational. 9 15))))

  (is (= (list 1 1) (make-rat 1)))
  (is (= (list 1 1) (make-rat 2)))
  (is (= (list 1 2) (make-rat 1 2)))

  (is (= (list 1 1) (make-rat 1)))
  (is (= (list 1 1) (make-rat 2)))
  (is (= (list 1 2) (make-rat 1 2)))
  (is (= (list 2 3) (make-rat 4 6)))

  (is (= (list 4 3) (add-rat (make-rat 4 6) (make-rat 4 6))))
  (is (= (list 1 1) (add-rat (make-rat 4 6) (make-rat 3 9))))
  (is (= (list 1 1) (mul-rat (make-rat 4 6) (make-rat 6 4)))))

(comment
  (let [l (list 1 2)]
    (second l))
  (mod 2 1)
  (mod 1 2)
  (gcd 2 3)
  (max 2 1))
