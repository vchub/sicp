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
  (def solve-dy nil)
  ;; (def solve-y (lazy-seq (integral-lazy solve-dy y0 dt)))
  (def solve-y (integral-lazy (lazy-seq (cons 0 solve-dy)) y0 dt))
  (def solve-dy (map f solve-y))
  solve-y

  ;; (letfn [(y [] (integral-lazy (lazy-seq (cons 0 (dy))) y0 dt))
  ;;         (dy [] (map f (y)))]
  ;;   (lazy-seq (cons y0 (y))))

  )

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
    (iter y0 1)))

(defn to-self [x0]
  (def to-self-y 0)
  (def to-self-x (lazy-seq (cons x0 to-self-y)))
  (def to-self-y [2])
  ;; (let [y 0
  ;;       x (lazy-seq (cons x0 (force y)))
  ;;       y (delay [2])
  ;;       ]
  ;;   x)
  to-self-x)


(defn square [x] (* x x))

(defn second-order-iter
  "(num)-> num, init, num -> Stream [num]
   solve diff.equation: dy^2/dt^2 = f(y)"
  [f y0 dt]
  (letfn [(iter [iiy iy dy]
            (lazy-seq (cons iiy (iter
                                ;; integral integral
                               (+ iiy (* iy dt))
                                ;; integral
                               (+ iy (* dy dt))
                                ;; dy
                               (f iiy)))))]
    (iter y0 0 0))
  )

(deftest test-delayed
  (testing "to-self"
    ;; (is (= [1 2] (to-self 1)))
    (is (= [1 2] (take 2 (to-self 1))))
    )

  (testing "solve"
    (is (close-enough Math/E (nth (solve identity 1 0.05) 20) 0.5))
    ;; (is (close-enough Math/E (nth (solve identity 1 1e-3) 1e3) 1e-2))
    (is (close-enough Math/E (nth (solve-recursion identity 1 0.05) 20) 0.5))
    (is (close-enough Math/E (nth (solve-iter identity 1 1e-4) 1e4) 5e-4))

    (is (close-enough Math/E (nth (solve-iter identity 1 1e-4) 1e4) 5e-4))

    ;; dy/dt = -y
    ;; (is (close-enough (Math/sin 1) (nth (solve-iter (fn[x] (* -1 x)) 1 1e-4) 1e4) 1e-2))

    ;; dy^2/dt^2 = -y
    (is (close-enough (Math/cos 1) (nth (second-order-iter (fn[x] (* -1 x)) 1 1e-4) 1e4) 1e-2))

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

(comment
  (let [x (delay nil)]
    (force x))
  (force 1)
  )
