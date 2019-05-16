(ns sicp.ch2.intervals
  (:require [clojure.test :refer :all]))

(defprotocol Arithmetic
  (add [x y])
  (sub [x y])
  (mult [x y])
  (div [x y])
  (reciprical [x])
  (neg [x])
  (width [x])
  (mid [x])
  )

(defrecord Interval [a b]
  Arithmetic
  (add [x y] (Interval. (+ (:a x) (:a y)) (+ (:b x) (:b y))))
  (neg [x] (Interval. (* -1 (:a x)) (* -1 (:b x)) ))
  (sub [x y] (add x (neg y)))
  (reciprical [x] (Interval. (/ 1 (:b x)) (/ 1 (:a x))  ))
  (mult [x y] (Interval. (* (:a x) (:a y)) (* (:b x) (:b y))))
  (width [x] (/ (- (:b x) (:a x)) 2))

  (div [x y] (cond
               (zero? (width y)) (throw (Exception. "|y| == 0"))
               :else (mult x (reciprical y))))
  (mid [x] (/ (+ (:a x) (:b x)) 2))
  )

(defn cent->interval [x dx] (Interval. (- x (* x dx)) (+ x (* x dx))))


(testing
  (let [x (Interval. 0 2)
        y (Interval. 1 2)]
    (is (= (Interval. 1 4) (add x y)))
    (is (= (Interval. -1 0) (sub x y)))
    (is (= (Interval. 1/2 1) (reciprical y)))
    (is (= (Interval. 0 4) (mult x y)))
    (is (= 1 (width x)))
    (is (= 3/2 (+ (width x) (width y))))
    (is (not (= (width (mult x y)) (* (width x) (width y)))))
    (is (= 3/2 (mid y)))
    (is (= (Interval. 0.9 1.1) (cent->interval 1 0.1)))
    ))

(comment
  (for [x [1 2] y [3 4]] (* x y))
  )
