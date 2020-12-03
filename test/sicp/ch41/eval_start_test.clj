(ns sicp.ch41.eval-start-test
  (:require [clojure.test :refer :all]
            [sicp.ch41.eval-start :as eval1]))

(deftest env-test
  (let [evall (fn [exp] (eval1/evall exp (atom (into {} @eval1/global-env))))]
    (testing "compound-proc"
      (is (= 10 (evall '(do
                          (def id (fn [x] x))
                          (id 10)))))

      (is (= 6 (evall '(do
                         (def add (fn [x y] (+ x y)))
                         (add 3 3)))))

      (is (= 9 (evall '(do
                         (def mul (fn [x y]
                                    (do
                                      ; (prn "x" x "y" y)
                                      (* x y))))
                         (mul 3 3)))))

      (is (= 24 (evall '(do
                          (def fact (fn [x]
                                      (if (< x 2)
                                        1
                                        (* x (fact (- x 1))))))
                          (fact 4)))))

      (is (= true (evall '(do (def a true) a))))
      (is (= false (evall '(do (def a false) a))))

      ; wrong and = or
      (is (= true (evall '(do
                            (set! and (fn [a b] (if a a b)))
                            (and true false))))))

    (testing "def, if, and, or, variable"

      (is (= false (evall '(and false false))))
      (is (= false (evall '(and false true))))
      (is (= false (evall '(and true false))))
      (is (= true (evall '(and true true))))

      (is (= 1 (evall '(if (< 1 2) 1 2))))
      (is (= 2 (evall '(if (> 1 2) 1 2))))
      (is (= 2 (evall '(if false 1 2))))
      (is (= 1 (evall '(if true 1 2))))

      (is (= 2 (evall '(if (and (< 0 2) (> 2 4)) (* 1 1) 2))))
      (is (= 1 (evall '(if (and (< 0 2) (< 2 4)) (* 1 1) 2))))
      (is (= 1 (evall '(if (or (< 0 2) (> 2 4)) (* 1 1) 2))))
      (is (= 2 (evall '(if (or (> 0 2) (> 2 4)) (* 1 1) 2))))
      (is (= 1 (evall '(if (or (> 0 2) (< 2 4)) (* 1 1) 2))))

      (is (= 3 (evall '(if (or true true) 3 2))))
      (is (= 3 (evall '(if (or true false) 3 2))))
      (is (= 2 (evall '(if (or false false) 3 2))))
      (is (= 2 (evall '(if false 3 2))))

      (is (= 1 (evall '(do
                         (def x 1)
                         x))))
      (is (= 3 (evall '(do
                         (def x 1)
                         (def y 2)
                         (+ x y)))))
      (is (thrown? Exception
                   (evall '(do
                             (def x 1)
                             (def x 2)
                             (+ x x)))))
      (is (thrown? Exception
                   (evall '(do
                             (def x 1)
                             (def x 2)
                             (+ x z)))))

      (is (= '(fn [x] x) (evall '(do
                                   (def id (fn [x] x))
                                   id)))))

    (testing "primitives"
      (is (= 1 (evall 1)))
      (is (= "foo" (evall "foo")))
      (is (= true (evall 'true)))
      (is (= false (evall 'false)))
      (is (= 2 (evall (quote (+ 1 1)))))
      (is (= 2 (evall '(+ 1 1))))
      (is (= 2 (evall '(* 2 1))))
      (is (= 1/2 (evall '(/ 1 2))))
      (is (= true (evall '(< 1 2))))
      (is (= true (evall '(= 1 1))))
      (is (= true (evall '(= "a" "a"))))
      (is (= false (evall '(= "b" "a"))))
      (is (= 6 (evall '(* (+ 2 1) (/ 2 1))))))))
