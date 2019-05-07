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
  (is (= (ex111 7) (ex111iter 7))))

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

;; num, [num] -> num
(def coins-num
  (memoize (fn [amount xs]
             (cond
               (< amount 0) 0
               (empty? xs) 0
               (= amount 0) 1
               :else (+ (coins-num (- amount (first xs)) xs) (coins-num amount (rest xs)))))))

(testing
 (is (= 0 (coins-num 1 [])))
  (is (= 1 (coins-num 1 [1])))
  (is (= 1 (coins-num 1 [1 2])))
  (is (= 2 (coins-num 2 [1 2])))
  (is (= 2 (coins-num 3 [1 2])))
  (is (= 3 (coins-num 3 [1 2 3])))
  (is (= 292 (coins-num 100 [1 5 10 25 50]))))

(defn ex112
  "int -> int
   A Pascal triangle element f(n,k) = f(n-1, k-1) + f(n-1, k)"
  [n k]
  (let [iter  (fn [i raw]
                ;; (prn i raw)
                (if (= i n)
                  (nth raw k)
                  (let [i (inc i)
                        cnth (fn [j]
                               (cond
                                 (zero? j) 1
                                 :else (+ (nth raw (dec j)) (nth raw j))))]

                    (recur i (vec (concat (map cnth (range i)) [1]))))))]

    (if (<= n 1)
      1
      (iter 1 [1 1]))))

(testing
 (is (= 1 (ex112 1 0)))
  (is (= 1 (ex112 2 0)))
  (is (= 2 (ex112 2 1)))
  (is (= 1 (ex112 3 0)))
  (is (= 3 (ex112 3 1)))
  (is (= 3 (ex112 3 2)))
  (is (= 1 (ex112 3 3)))
  (is (= 4 (ex112 4 3)))
  (is (= 6 (ex112 4 2))))

(defn exponent [a n]
  (cond
    (= 0 n) 1
    (= 1 n) a
    :else
    (let [x (exponent a (int (/ n 2)))
          y (exponent a (mod n 2))]
      (* x x y))))

(defn square [x] (* x x))

(defn exponent1 [a n]
  (cond
    (zero? n) 1
    (= 1 n) a
    (even? n) (square (exponent1 a (/ n 2)))
    :else
    (* (exponent1 a (dec n)) a)))

(defn fast-exp-iter [b n]
  (loop [b b n n a 1]
    ;; invariant a * b^n
    (cond
      (= n 0) 1
      (= n 1) (* b a)
      (odd? n) (recur (square b) (int (/ n 2)) b)
      :else
      (recur (square b) (int (/ n 2)) 1))))

(testing

 (doseq [f [exponent exponent1 fast-exp-iter]]
   (is (= 1 (f 2 0)))
   (is (= 4 (f 2 2)))
   (is (= 8 (f 2 3)))
   (is (= 9 (f 3 2)))
   (is (= 27 (f 3 3)))))

(defn gcd
  "gcd(a,b) = gcd(b, a%b)"
  [a b]
  (let [r (mod a b)]
    (if (zero? r)
      b
      (recur b r))))

(testing
 (is (= 1 (gcd 1 1)))
  (is (= 2 (gcd 4 2)))
  (is (= 4 (gcd 28 16))))

(defn smallest-divisor
  [n]
  (loop [i 2]
    (cond
      (> (square i) n) n
      (zero? (mod n i)) i
      :else
      (recur (inc i)))))

(defn prime?
  [n]
  (= n (smallest-divisor n)))

(testing
  (is (= 1 (smallest-divisor 1)))
  (is (= 2 (smallest-divisor 2)))
  (is (= 2 (smallest-divisor 4)))
  (is (= 3 (smallest-divisor 3)))
  (is (= 2 (smallest-divisor 6)))
  (is (= 3 (smallest-divisor 15)))
  (is (not= 5 (smallest-divisor 15)))
  (is (prime? 2))
  (is (not (prime? 4)))
  (is (not (prime? 93)))
  (is (prime? 23))
  )

(defn expmod
  "a^n mod m
   invariant: a^n mod m * b^n mod m = (a*b)^n mod m"
  [base exp m]
  (cond
    (zero? exp) 1
    (even? exp) (mod (square (expmod base (/ exp 2) m)) m)
    :else (mod (* base (expmod base (dec exp) m)) m)
    )
  )

(defn ferma-test [n]
  (let [a (inc (rand-int (dec n)))]
    (= a (expmod a n n))))

(defn fast-prime? [n times]
  (loop [times times]
    (cond
    (zero? times) true
    (ferma-test n) (recur (dec times))
    :else false)))

(testing
  (is (= 1 (expmod 2 3 7)))
  (is (= 2 (expmod 2 4 7)))
  (is (fast-prime? 3 2))
  (is (not (fast-prime? 12 2)))
  (is (not (fast-prime? 22 2)))
  (is (fast-prime? 23 2))
  (is (not (fast-prime? 51 2)))
  (is (fast-prime? 53 2))
  )

(comment
  (rand-int 10)
  (let [a (/ 12.15 0.1)
        log (fn [x] (Math/log x))]
    (/ (log a) (log 3)))

  (flatten
   (map-indexed (fn [i x] [i x]) (range 1 5)))
  (concat (range 3) [1])
  (vec (list 1 2))
  (mod 5 2)
  (rem 5 2)
  (/ 3. 4.)
  (< (/ 3 4) 1)
  (Math/sqrt 3))
