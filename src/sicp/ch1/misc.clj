(ns sicp.ch1.misc
  (:require [clojure.test :refer :all]))

;; (defn >=
;;   "doc text"
;;   [x y]
;;   (or (> x y) (= x y)))

(defn sqr [x] (* x x))
(defn abs [x] (if (< x 0) (* -1 x) x))
(defn abs-diff [x y] (abs (- x y)))

(defn inverse
  "root of inverse of f at y by Newton's method"
  [f df eps y x0]
  (let [_f (fn [x] (- (f x0) y))]
    (cond
      (> eps (abs (_f x0))) x0
      :else
      (recur f df eps y (- x0 (/ (_f x0) (df x0)))))))

(defn sqrt [y eps]
  (let [f (fn [x] (sqr x))
        df (fn [x] (* 2 x))
        x0 1]
    (inverse f df eps y x0)))

(defn cube-root [y eps]
  (let [f (fn [x] (* x x x))
        df (fn [x] (* 3 (sqr x)))
        x0 1]
    (inverse f df eps y x0)))

(testing
 (is (= true (>= 3 3)))
  (is (= 2 (abs 2)))
  (is (= 2 (abs -2)))
  (is (= 4 (sqr 2)))
  (let [eps 0.05]
    (is (> eps (abs-diff 2 (sqrt 4 eps))))
    (is (> (Math/pow eps 2) (abs-diff (Math/sqrt 3) (sqrt 3 eps))))
    (is (> (Math/pow eps 2) (abs-diff (Math/pow 3 (/ 1 3)) (cube-root 3 eps))))))

(comment
  (mod 5 2)
  (rem 5 2)
  (/ 3. 4.)
  (< (/ 3 4) 1)
  (Math/sqrt 3))
