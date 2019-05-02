(ns sicp.ch1.misc
  (:require [clojure.test :refer :all]))

;; (defn >=
;;   "doc text"
;;   [x y]
;;   (or (> x y) (= x y)))

(defn sqr [x] (* x x))
(defn abs [x] (if (< x 0) (* -1 x) x))
(defn abs-diff [x y] (abs (- x y)))

(defn ex111
  "int -> int
   A function f is defined by the rule that f(n) = n if n<3 and f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n> 3. "
  [n]
  (cond
    (< n 3) n
    :else
    (+ (ex111 (- n 1)) (* 2 (ex111 (- n 2))) (* 3 (ex111 (- n 3))))))

(defn ex111iter
  "int -> int
   A function f is defined by the rule that f(n) = n if n<3 and f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n> 3. "
  [n]
  (let [iter  (fn [f-1 f-2 f-3 i]
                (let [fi (+ f-1 (* 2 f-2) (* 3 f-3))]
                  (cond
                    (= i n) fi
                    :else
                    (recur fi f-1 f-2 (+ i 1)))))]

    (if (< n 3)
      n
      (iter 2 1 0 3))))

(testing
 (is (= 0 (ex111 0)))
  (is (= 1 (ex111 1)))
  (is (= 4 (ex111 3)))
  (is (= 4 (ex111iter 3)))
  (is (= (ex111 5) (ex111iter 5)))
  (is (= (ex111 7) (ex111iter 7)))
  )

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

(defn coins-num
  "num, [num] -> num"
  [amount xs]
  (cond
    (< amount 0) 0
    (empty? xs) 0
    (= amount 0) 1
    :else (+ (coins-num (- amount (first xs)) xs) (coins-num amount (rest xs)))))

(testing
  (is (= 0 (coins-num 1 [])))
  (is (= 1 (coins-num 1 [1])))
  (is (= 1 (coins-num 1 [1 2])))
  (is (= 2 (coins-num 2 [1 2])))
  (is (= 2 (coins-num 3 [1 2])))
  (is (= 3 (coins-num 3 [1 2 3])))
  )

(comment
  (mod 5 2)
  (rem 5 2)
  (/ 3. 4.)
  (< (/ 3 4) 1)
  (Math/sqrt 3))
