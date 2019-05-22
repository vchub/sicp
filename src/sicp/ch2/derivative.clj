(ns sicp.ch2.derivative
  (:require [clojure.test :refer :all]))

(defn variable? [x] (symbol? x))
;; (defn same-var? [x y] (= x y))
(defn same-var? [x y] (and (symbol? x) (symbol? y) (= x y)))

(defn num0? [x] (and (number? x) (zero? x)))
(defn num1? [x] (and (number? x) (= 1 x)))

;; (defn make-sum [x y]
;;  (cond
;;    (num0? x) y
;;    (num0? y) x
;;    (and (number? x)(number? y)) (+ x y)
;;    :else (list '+ x y)
;;    ))

(defn make-sum [xs]
  (let [num-sum (reduce + 0 (filter number? xs))
        syms (filter (complement number?) xs)]
    (cond
      (zero? num-sum) (cond
                        (empty? syms) 0
                        (= 1 (count syms)) (first syms)
                        :else (cons '+ syms))
      (empty? syms) num-sum
      :else (concat '(+) [num-sum] syms))))

(defn make-prod [xs]
  (let [num-sum (reduce * 1 (filter number? xs))
        syms (filter (complement number?) xs)]
   ;; (prn num-sum syms)
    (cond
      (zero? num-sum) 0
      (= 1 num-sum) (cond
                      (empty? syms) 1
                      (= 1 (count syms)) (first syms)
                      :else (cons '* syms))
      (empty? syms) num-sum
      :else (concat '(*) [num-sum] syms))))

(defn make-expon [base pow]
  (cond
    (zero? pow) 1
    (= 1 pow) base
    (symbol? base) (list '** base pow)
    (and (number? base) (number? pow)) (Math/pow base pow)
    :else (throw (Exception. (str "unknown exponent expression " base pow)))))

(defn sum? [e] (and (list? e) (= '+ (first e))))
(defn product? [e] (and (list? e) (= '* (first e))))
(defn expon? [e] (and (list? e) (= '** (first e))))
(def a1 second)
(defn a2 [e] (rest e))
(defn third [e] (nth e 2))

(def prod-deriv)

(defn deriv
  [e x]
  (let [dx (fn [e] (deriv e x))]
    (cond
      (number? e) 0
      (variable? e) (if (same-var? e x) 1 0)
      (sum? e) (make-sum (map dx (a2 e)))
      (product? e) (prod-deriv (a2 e) x)
      (expon? e) (make-prod [(third e) (make-expon (a1 e) (dec (third e)))])
      :else (throw (Exception. (str "unknown expression " e))))))

(defn rm [x xs]
  (loop [t xs acc []]
    (cond
      (empty? t) acc
      (= x (first t)) (concat acc (rest t))
      :else (recur (rest t) (conj acc (first t))))))

(defn prod-deriv [xs x]
  (make-sum (map (fn [y] (make-prod (cons (deriv y x) (rm y xs)))) xs)))

(testing
 (is (expon? '(** b 2)))
  (is (= '(** a 2) (make-expon 'a 2)))
  ;; (is (= '(** a b) (make-expon 'a 'b)))
  (is (= 8.0 (make-expon 2 3)))
  (is (= '(* 2 x) (deriv '(** x 2) 'x)))
  (is (= '(* 3 (** x 2)) (deriv '(** x 3) 'x)))

  (is (= 'y (prod-deriv '(x y) 'x)))
  (is (= 3 (prod-deriv '(x 3) 'x)))
  (is (= 3 (prod-deriv '(3 x) 'x)))
  (is (= '(+ x x) (prod-deriv '(x x) 'x)))
  (is (= 0 (prod-deriv '(3 y) 'x)))
  (is (= 0 (prod-deriv '(3 y z) 'x)))
  (is (= '(+ 3 x y) (make-sum '(x 3 y))))
  (is (= '(+ 3 y) (make-sum '(3 y))))

  (is (= '(* 3 y) (prod-deriv '(x 3 y) 'x)))
  (is (= '(* 3 y z) (prod-deriv '(x 3 y z) 'x)))
  (is (= '(+ (* 3 y x z) (* 3 y x  z)) (prod-deriv '(x 3 y x z) 'x)))

  (is (= 0 (make-prod '(y 0))))
  (is (= '(3 y) (rm 'x '(x 3 y))))
  (is (= '(x 3) (rm 'y '(x 3 y))))
  (is (= '(x 3 y) (rm 'y '(x 3 y y))))

  (is (= 1 (deriv '(+ x 3 4) 'x)))
  ;; (is (= 6 (deriv '(+ (* 3 x) (* 2 x) x) 'x)))

  (is (= '(1 2) (a2 '(* 1 2))))
  (is (= '(1 2 x) (a2 '(* 1 2 x))))
  (is (= 0 (deriv 3 'x)))
  (is (= 1 (deriv '(+ x 3) 'x)))
  (is (product? '(* x 3)))

  (is (= '(* 3 x y) (make-prod '(x 3 y))))
  (is (= '(* 3 x) (make-prod '(x 3))))
  (is (= 'x (make-prod '(x))))
  (is (= 12 (make-prod '(4 3 1))))

  (is (= 3 (deriv '(* x 3) 'x)))
  (is (= 4 (deriv '(+ (* 3 x) (+ 2 x)) 'x)))
  (is (= 5 (deriv '(+ (* 3 x) (* 2 x)) 'x)))
  (is (= '(+ 3 (* 2 y)) (deriv '(+ (* 3 x) (* 2 x y)) 'x))))

(comment
  (nth [1 2 3] 0))
