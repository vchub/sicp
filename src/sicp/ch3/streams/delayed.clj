(ns sicp.ch3.streams.delayed
  (:require [clojure.test :refer :all]
            [sicp.ch3.streams.stream-cons :refer [cons-m car cdr take-s stream-ref] :as ss]
            [sicp.ch3.streams.infinite :refer [close-enough partial-sums abs] :as inf]
            [sicp.ch3.streams.signals :refer [integers ones add-streams scale-stream]]))

(defn integral "Stream, num, num -> Stream"
  [xs init dt]
  (cons-m init (do
                 (prn (car xs))
                 (add-streams (scale-stream dt xs) (integral xs init dt)))))


;; (defn scale-s [dt] (map #(* % dt)))
;; (def add-s (map +))
;; (def dbl (map #(* 2 %)))


(defn integral-lazy "Stream, num, num -> Stream"
  [xs init dt]
  (lazy-seq (cons init (do
                         ;; (prn (first xs))
                         (map + (map #(* dt %) xs)
                              (integral-lazy xs init dt))))))

(defn solve
  "(num)-> num, init, num -> Stream [num]
   solve diff.equation: dy/dt = f(y)"
  [f y0 dt]
  (letfn [(y [] (integral-lazy (lazy-seq (cons 0 (dy))) y0 dt))
          (dy [] (map f (y)))]
    (lazy-seq (cons y0 (y)))))

(defn solve-recursion
  "(num)-> num, init, num -> Stream [num]
   solve diff.equation: dy/dt = f(y)"
  [f y0 dt]
  (integral-lazy (lazy-seq (cons 0 (map f (solve-recursion f y0 dt)))) y0 dt))

(defn solve-iter
  "(num)-> num, init, num -> Stream [num]
   solve diff.equation: dy/dt = f(y)"
  [f y0 dt]
  (letfn [(iter [y dy]
            (lazy-seq (cons y (iter
                                ;; integral
                               (+ y (* dy dt))
                                ;; dy
                               (f y)))))]
    (iter y0 (f y0))))

(defn square[x] (* x x))

(deftest test-delayed
  (testing "solve"
    (is (close-enough Math/E (nth (solve identity 1 0.05) 20) 0.5))
    (is (close-enough Math/E (nth (solve-recursion identity 1 0.05) 20) 0.5))
    (is (close-enough Math/E (nth (solve-iter identity 1 1e-3) 1e3) 5e-3))
    ;; dy/dt = 1/cos^2(y)
    ;; (is (close-enough (Math/tan 1)
    ;;                   (nth (solve-iter #(/ 1 (square (Math/cos %))) 0 1e-3) 1e3) 1e-3))
    )
  ;; (testing "recursion"
  ;;   (let [int-s (fn int-s [n] (cons-m n (do (prn n) (int-s (inc n)))))
  ;;         s (int-s 1)]
  ;;     (is (= [1 2 3] (take-s s 3)))
  ;;     (prn "---")
  ;;     (is (= [1 2 3 4] (take-s s 4)))
  ;;     ))

  ;; (testing "integral"
  ;;   (let [i1 (integral integers 0 1)
  ;;         i2 (integral-lazy (range) 0 1)]
  ;;     (is (= 6 (stream-ref i1 4)))
  ;;     (prn "---")
  ;;     (is (= 10 (stream-ref i1 5)))
  ;;
  ;;     (prn "---")
  ;;     (is (= 6 (nth i2 4)))
  ;;     (prn "---")
  ;;     (is (= 10 (nth i2 5)))))
  )

(test-delayed)
