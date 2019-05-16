(ns sicp.ch1.ex14
  (:require [clojure.test :refer :all]
            [sicp.ch1.ch_1_3 :as ch13 :refer [square]]))

(defn cubic
  [a b c]
  (fn [x] (+ (* x x x) (* a  x x) (* b x) c)))

(defn cubic-root
  [a b c dx]
  (ch13/newton-method (cubic a b c) 1 dx))

(testing
 (is (= 1 ((cubic 1 1 1) 0)))
  (is (ch13/close-enough -1 (cubic-root 1 1 1 0.1) 0.1)))

(defn double-f [f] (fn [x] (f (f x))))

(testing
 (is (= 21 (((double-f (double-f double-f)) inc) 5))))

;; ex 1.42


(testing
 (let [compose (fn [f g] (fn [x] (f (g x))))]
   (is (= 49 ((compose ch13/square inc) 6)))))

;; ex 1.43


(testing
 (let [repeated (fn [f n]
                  (fn [x]
                    (loop [acc x n n]
                      (if (<= n 0) acc (recur (f acc) (dec n))))))]

   (is (= 625 ((repeated square 2) 5)))
   (is (= (last (take 3 (iterate inc 5))) ((repeated inc 2) 5)))))

(defn iterative-improve
  "(num)->num,(num)->num -> (num)-num"
  [good? improve]
  (fn [x]
    (if (good? x) x (recur (improve x)))))

(defn fixed-point
  "(num)->num, num, num -> num"
  [f x dx]
  (let [good? (fn [x] (< (ch13/abs (- (f x) x)) dx))]
    ((iterative-improve good? f) x)))

(defn sqrt-iterative-improve
  "num -> num"
  [x dx]
  (fixed-point (fn [y] (/ (+ y (/ x y)) 2)) 1 dx))

(defn sqrt-iterative-improve-newton
  "num -> num"
  [x dx]
  (let [improve (ch13/newton-transform (fn [y] (- (square y) x)) dx)
        good? (fn [y] (< (ch13/abs (- (square y) x)) dx))]
    ((iterative-improve good? improve) 1)))

(testing
 (is (ch13/close-enough (Math/sqrt 2) (sqrt-iterative-improve 2 0.01) 0.01))
  (is (ch13/close-enough (Math/sqrt 2) (sqrt-iterative-improve-newton 2 0.01) 0.01)))
