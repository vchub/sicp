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

(defn mul-const "Stream, num -> Stream"
  [es x]
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
    :else (let [c (+ (:c (car xs)) (:c (car ys)))]
            (cons-m {:p (:p (car xs)) :c c} (add (cdr xs) (cdr ys))))))

(defn mul-terms "Stream, {:p :c} -> Stream"
  [x y]
  {:p (+ (:p x) (:p y)) :c (* (:c x) (:c y))})

(defn mul-term-series "Stream, {:p :c} -> Stream"
  [x es]
  (ss/map-stream #(mul-terms x %) es))

(defn mul [xs ys]
  (if (or (nil? xs) (nil? ys))
    nil
    (cons-m (mul-terms (car xs) (car ys))
            (add
             (mul-term-series (car xs) (cdr ys))
             (mul (cdr xs) ys)))))

(defn invert-unit-s "Stream 1 + ... -> Stream"
  [xs]
  (cons-m {:p 0 :c 1} (mul (mul-const (cdr xs) -1) (invert-unit-s xs))))

(defn tan [x]
  (series-partial-sum (mul sin-s (invert-unit-s cos-s)) x))

(deftest test-series
  (testing "invert-unit-s"
    (let [x (cons-m {:p 0 :c 1} (cons-m {:p 1 :c 1} nil))
          exp-1 (fn [x] (series-partial-sum (invert-unit-s exp-s) x))]
      (is (= [{:p 0 :c 1} {:p 1 :c -1} {:p 2 :c 1} {:p 3 :c -1}] (take-s (invert-unit-s x) 4)))
      (is (close-enough (/ 1 (Math/exp 0.2)) (stream-ref (exp-1 0.2) 4) 1e-3)))
    (is (close-enough (Math/tan 0.3) (stream-ref (tan 0.3) 4) 1e-3)))

  (testing "mul"
    (is (= (take-s (mul-const exp-s 2) 10) (take-s (mul-term-series {:p 0 :c 2} exp-s) 10)))
    (is (= (take-s (mul-const exp-s 2) 10) (take-s (mul (cons-m {:p 0 :c 2} nil) exp-s) 10)))
    (is (= (take-s (mul-const exp-s 2) 10) (take-s (mul exp-s (cons-m {:p 0 :c 2} nil)) 10)))
    (let [x (cons-m {:p 0 :c 2} (cons-m {:p 1 :c 1} nil))
          y (cons-m {:p 0 :c 2} (cons-m {:p 1 :c 1} nil))]
      (is (= [{:p 0 :c 4} {:p 1 :c 2}] (take-s (add x y) 2)))
      (is (= [{:p 0 :c 4} {:p 1 :c 4} {:p 2 :c 1}] (take-s (mul x y) 4))))

    ;; (is (= 1 (take-s (mul exp-s exp-s) 4)))


    ;; sin^2
    (is (close-enough (Math/pow (Math/sin 0.5) 2)
                      (stream-ref (series-partial-sum (mul sin-s sin-s) 0.5) 4) 1e-3))
    ;; e*sin^2
    (is (close-enough (* (Math/exp 0.5) (Math/pow (Math/sin 0.5) 2))
                      (stream-ref (series-partial-sum (mul exp-s (mul sin-s sin-s)) 0.5) 5) 1e-3))
    ;; sin^2 + cos^2 = 1
    (is (= [{:p 0 :c 1} {:p 2 :c 0} {:p 4 :c 0} {:p 6 :c 0}]
           (take-s (add (mul sin-s sin-s) (mul cos-s cos-s)) 4))))

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
