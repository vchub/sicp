(ns sicp.ch3.streams.series
  (:require [clojure.test :refer :all]
            [sicp.ch3.streams.stream-cons :refer [cons-m car cdr take-s stream-ref] :as ss]
            [sicp.ch3.streams.infinite :refer [close-enough partial-sums]]))

(def exp-s
  (letfn ([iter [e i] (cons-m e (iter {:p i :c (/ (:c e) i)} (inc i)))])
    (iter {:p 0 :c 1} 1)))

(defn series-partial-sum [es x]
  (letfn [(iter [es acc]
            (cons-m acc
                    (iter
                     (cdr es)
                     (let [p (:p (car es))
                           c (:c (car es))
                           xp (Math/pow x p)]
                       (+ acc (* c xp))))))]
    (iter es 0)))

(defn exp [x] (series-partial-sum exp-s x))

(def cos-s
  (letfn ([iter [e i] (cons-m e (iter {:p i :c (* -1 (/ (:c e) (dec i) i))} (+ i 2)))])
    (iter {:p 0 :c 1} 2)))

(defn cos [x] (series-partial-sum cos-s x))

(defn differentiate "Stream -> Stream"
  [es]
  (let [p (:p (car es))
        c (:c (car es))]
    (if (or (zero? p) (zero? c))
      (recur (cdr es))
      (cons-m {:p (dec p) :c (* p c)} (differentiate (cdr es))))))

(defn integrate "Stream -> Stream"
  [es]
  (let [p (inc (:p (car es)))
        c (:c (car es))]
    (if (zero? c)
      (recur (cdr es))
      (cons-m {:p p :c (/ c p)} (integrate (cdr es))))))

(defn mul-const "Stream, num -> Stream" [es x]
  (ss/map-stream #(update % :c (fn [c] (* c x))) es))

(def sin-s (mul-const (differentiate cos-s) -1))

(defn sin [x]
  (letfn ([iter [e i]
           (cons-m e (iter {:p i :c (* -1 (/ (:c e) (dec i) i))} (+ i 2)))])
    (series-partial-sum (iter {:p 1 :c 1} 3) x)))

(defn sin1 [x] (series-partial-sum sin-s x))

(defn add [xs ys]
  (cond
    (nil? xs) ys
    (nil? ys) xs
    (< (:p (car xs)) (:p (car ys))) (cons-m (car xs) (add (cdr xs) ys))
    (> (:p (car xs)) (:p (car ys))) (cons-m (car ys) (add xs (cdr ys)))
    :else (cons-m {:p (:p (car xs)) :c (+ (:c (car xs)) (:c (car ys)))} (add (cdr xs) (cdr ys)))))

(defn mul [xs ys]
  (cons-m {:p (+ (:p (car xs)) (:p (car ys))) :c (* (:c (car xs)) (:c (car ys)))}
          (add (mul (cons-m (car xs) nil) (cdr ys))
               (when (and (cdr xs) (cdr ys))
                 (mul (cdr xs) (cdr ys)))))
  )

(deftest test-series
  (testing "mul"
    (is (= (take-s exp-s 10) (take-s (differentiate exp-s) 10)))
    (is (close-enough (Math/pow (Math/sin 0.5) 2)
                      (stream-ref (series-partial-sum (mul sin-s sin-s) 0.5) 4) 1e-3))
    )

  (testing "add"
    (is (= (take-s (mul-const cos-s 2) 10) (take-s (add cos-s cos-s) 10)))
    (is (close-enough (* 2 (Math/sin 0.5))
                      (stream-ref (series-partial-sum (add sin-s sin-s) 0.5) 3) 1e-3)))

  (testing "e"
    (is (= [{:p 0 :c 1} {:p 1 :c 1} {:p 2 :c 1/2} {:p 3 :c 1/6}] (take-s exp-s 4)))
    (is (close-enough Math/E  (stream-ref (exp 1) 7) 1e-3))
    (is (close-enough (Math/exp 0.5)  (stream-ref (exp 0.5) 5) 1e-3))
    (is (= (take-s exp-s 10) (take-s (differentiate exp-s) 10)))
    (is (= (take-s exp-s 10) (take-s (cons-m {:p 0 :c 1} (integrate exp-s)) 10))))

  (testing "cos, sin"
    (is (close-enough (Math/cos 0.5)  (stream-ref (cos 0.5) 3) 1e-3))
    (is (close-enough (Math/sin 0.5)  (stream-ref (sin 0.5) 3) 1e-3))
    (is (close-enough (Math/sin 1.5)  (stream-ref (sin 1.5) 5) 1e-3))
    (is (= (take-s cos-s 8) (take-s (differentiate sin-s) 8)))
    (is (close-enough (Math/sin 0.5)  (stream-ref (sin1 0.5) 5) 1e-3)))
  (is (= (take-s cos-s 8) (take-s (cons-m {:p 0 :c 1} (mul-const (integrate sin-s) -1)) 8)))
  (is (= (take-s sin-s 8) (take-s (integrate cos-s) 8))))

(test-series)
