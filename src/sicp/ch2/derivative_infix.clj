(ns sicp.ch2.derivative-infix
  (:require [clojure.test :refer :all]
            [sicp.ch2.derivative :as deriv]))


;; (defn sum? [e] (and (list? e) (= '+ (second e))))
;; (defn product? [e] (and (list? e) (= '* (second e))))
;; (defn expon? [e] (and (list? e) (= '** (second e))))
;; (defn third [e] (nth e 2))


(defn partition-on [e token]
  (loop [e e curr [] acc []]
    (cond
      (empty? e) (conj acc curr)
      (= token (first e)) (recur (rest e) [] (conj acc curr))
      :else (recur (rest e) (conj curr (first e)) acc))))

(defn sum-tokens [e]
  (partition-on e '+))

(defn prod-tokens [e]
  (partition-on e '*))

(testing
 (is (= '(()) (sum-tokens '())))
  (is (= '([a]) (sum-tokens '(a))))
  (is (= '([1]) (sum-tokens '(1))))
  (is (= '([a] [1]) (sum-tokens '(a + 1))))
  (is (= '([a] (b * c)) (sum-tokens '(a + b * c))))
  (is (= '([a] (b * c * d) [e]) (sum-tokens '(a + b * c * d + e))))
  (is (= '([a] [1]) (prod-tokens '(a * 1)))))

(defn transpile [e]
  (cond
    (number? e) e
    (symbol? e) e
    (empty? e) (throw (Exception. (str "transpile got empty? list: " e)))
    (= 1 (count e)) (transpile (first e))
    :else
    (let [e (sum-tokens e)]
      (if (< 1 (count e))
        (cons '+ (map transpile e))
        (let [e (prod-tokens (first e))]
          (if (< 1 (count e))
            (cons '* (map transpile e))
            (transpile (first e))))))))

(testing
;;  (is (= '(()) (transpile '())))
 (is (= 'a (transpile '(a))))
  (is (= 1 (transpile '(1))))
  (is (= '(+ a 1) (transpile '(a + 1))))
  (is (= '(+ a 1 b) (transpile '(a + 1 + b))))
  (is (= '(* a 1 b) (transpile '(a * 1 * b))))
  (is (= '(+ a (* 1 b)) (transpile '(a + 1 * b))))
  (is (= '(+ a (* b c d) 2) (transpile '(a + b * c * d + 2))))
  (is (= '(+ a (* b c d) (* 2 3)) (transpile '(a + b * c * d + 2 * 3)))))

(testing
 (is (= 1 (deriv/deriv '(+ x y) 'x)))
  (is (= '(+ x y) (transpile '(x + y))))
  (is (= 1 (deriv/deriv (transpile '(x + y)) 'x)))
  (is (= '(+ 1 y) (deriv/deriv (transpile '(x + y * x)) 'x))))
