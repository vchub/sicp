(ns sicp.ch1.newton
  (:require [clojure.test :refer :all]))

;; the essence of Newton's method application:
;;   we need to find f at x
;;     we know derivative f at any point
;;     and we can "easily" calculate inverse (inv) f at any point
;;   we use recurrence y1 = y0 - (x - inv_f(y0)) / df(inv_f(y0))
;;   until abs(y1 - y0) > epsilon - some arbitrary precision


(defn abs [x] (if (< x 0) (* -1 x) x))
(defn sqr [x] (* x x))
;; (defn abs-diff [x y] (abs (- x y)))


(defn newton
  "number, fn, fn, number, number -> number
   root of inverse by newton method "
  [x inverse dF guess epsilon]
  (loop [guess guess]
    ;; (prn guess)
    (let [fy (inverse guess)
          delta (- fy x)
          df (dF guess)]
      (cond
        (< (abs delta) epsilon) guess
        :else (recur (- guess (/ delta df)))))))

(defn sqrt "square root" [x epsilon]
  (newton x (fn [x] (* x x)) (fn [x] (* 2 x)) 1 epsilon))

(defn cubert "cube root" [x epsilon]
  (newton x (fn [x] (* x x x)) (fn [x] (* 3 (sqr x))) 1 epsilon))

(testing
 (is (= 1 (abs 1)))
  (is (= 1 (abs -1)))
  (is (= 1.0 (abs -1.0)))
  (is (> 1e-2 (abs (- (Math/sqrt 2) (sqrt 2 1e-2)))))
  (is (> 1e-4 (abs (- (Math/sqrt 2) (sqrt 2 1e-3)))))
  (is (> 1e-4 (abs (- (Math/pow 2 1/3) (cubert 2 1e-3))))))

(comment
  (sqrt 2 0.01))
