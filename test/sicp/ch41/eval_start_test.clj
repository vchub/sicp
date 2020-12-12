(ns sicp.ch41.eval-start-test
  (:require [clojure.test :refer :all]
            [sicp.ch41.eval-start :as eval1]))

(deftest turing
  "Turing's Halting Theorem
   halts? says if (proc args) stops (halts)
   try-f - use halts? as subrotine"

  (defn halts? [proc args] args)
  (defn run-forever [] (run-forever))

  (defn try-f [proc args] (if (halts? proc args)
                            'run-forever
                            'halts))

  (is (= 'run-forever (try-f try-f true)))
  (is (= 'halts (try-f try-f false))))

(deftest env-test
  (let [new-env (fn [] (atom (into {} @eval1/global-env)))
        evall (fn [exp] (eval1/evall exp (new-env)))]

    (testing "non strict if, deivatives"
      (is (= 'cool (evall '(if true 'cool (/ 1 0)))))
      (is (thrown? ArithmeticException
                   (evall '(if false 'cool (/ 1 0)))))
      (is (= 1 (evall '(let [unless (fn [cnd a b]
                                      (if cnd b a))]
                         (unless false 1 2)))))
      (is (thrown? ArithmeticException (evall '(let [unless (fn [cnd a b]
                                                              (if cnd b a))]
                                                 (unless false 1 (/ 1 0)))))))

    (testing "fn parameters but not arguments"
      (is (thrown? ArithmeticException (evall '(let [f (fn [a b] a)] (f 'cool (/ 1 0))))))
      (is (= 'cool (evall '(let [f (fn [a (/ 1 0)] a)] (f 'cool 1))))))

    (testing "order of evalution. ex 4.19"
      (is (= 12 (let [a 1]
                  (do (def f (fn [x]
                               (def b (+ a x))
                               (def a 5)
                               (+ a b)))
                      (f 10)))))

      (is (= 1 (evall '(let [a 1] a))))

      (is (= 3 (evall '(do
                         (def f (fn [x] x)
                           (def b (+ 2 x))
                           b)
                         (f 3)))))

      (is (= 11 (evall '(let [a 1]
                          (def f (fn [x] (+ a x)))
                          (f 10)))))

      (is (= 16 (evall '(let [a 1]
                          (do (def f (fn [x]
                                       (def b (+ a x))
                                       (def a 5)
                                       (+ a b)))
                              (f 10)))))))

    (testing "mutual recursion"
      (is (= true (evall '(do
                            (def f (fn [x]
                                     ; (def odd?_ nil)
                                     ; (def even?_ nil)
                                     (def odd?_ (fn [x] (if (= 0 x) false (even?_ (- x 1)))))
                                     (def even?_ (fn  [x] (if (= 0 x) true (odd?_ (- x 1)))))

                                     (odd?_ x)))
                            (f 5))))))

    (testing "closure"
      (is (= 5 (evall '(do
                         (def cnt 0)
                         (def runner (fn [n]
                                       (if (> n 0)
                                         (do
                                           (set! cnt (+ cnt 1))
                                           (runner (- n 1)))
                                         nil)))
                         (runner 5)
                         cnt)))))

    (testing "let and fn with seq of exps"
      (is (= (list 1 1) (evall '(cons 1 '(1)))))
      (is (= (list 1 2) (evall '(cons 1 (cons (+ 1 1) nil)))))
      (is (= (list 1 2) (evall '(list 1 2))))
      (is (= (list 1 2) (evall '(list 1 (+ 1 1)))))
      (is (= 2 (evall '((fn [x] (+ x 1)) 1))))

      (is (= 6 (evall '((fn [x y]
                          (+ x y)
                          (/ x y)
                          (* x y)) 2 3))))

      (is (= 2 (evall '(let [x 1] (+ 1 x)))))
      (is (= 6 (evall '(let [x 1 y 2 z 3] (+ x y z)))))
      (is (= 13 (evall '(let [x 2 y (* x 2) z (+ y 3)] (+ x y z)))))
      (is (= 4 (evall '(let [f (fn [x] (* 2 x))] (f 2)))))
      (is (= 6 (evall '(let [fact (fn [x] (if (< x 2) 1 (* x (fact (- x 1)))))] (fact 3)))))

      (is (= 8 (evall '(do
                         (def fib (fn [n]
                                    (let-loop fib-iter [(a 1) (b 0) (count n)]
                                              (if (= count 0)
                                                b
                                                (fib-iter (+ a b)
                                                          a
                                                          (- count 1))))))
                         (fib 6)))))

      (is (= 5 (evall '(do
                         (def fib (fn [n] (cond
                                            (= n 0) 0
                                            (= n 1) 1
                                            true (+ (fib (- n 1)) (fib (- n 2))))))
                         (fib 5))))))

    (testing "&args"
      (let [f (fn [& args] (do
                            ; (prn args)
                             (count args)))]
        (is (= 0 (f)))
        (is (= 2 (f 1 2)))
        (is (= 3 (f 1 2 3)))
        (is (= 2 (f 1 '(2 3))))))

    (testing "evall, cond macros"
      (is (= 'foo (evall '(evall 'foo))))
      (is (= 2 (evall '(evall (+ 1 1)))))
      (is (= 2 (evall '(evall (if (< 1 0) 1 2)))))
      (is (= 2 (evall '(evall (if (< 1 0) 1 (evall (+ 1 1)))))))
      ; (is (= nil  (evall '(my-cond-m false 1))))
      )

    (testing "quote, unquote, cond"
      (is (= 'foo (evall ''foo)))
      (is (= true (evall 'true)))
      (is (= true (evall '(unquote-l 'true))))

      (is (= nil  (evall '(cond
                            false 1))))
      (is (= nil  (evall '(cond
                            (> 0 1) (* 1 2)))))
      (is (= 2 (evall '(do
                         (def x 2)
                         (cond
                           (+ x x) (* 1 2))))))
      (is (= 3  (evall '(cond
                          (> 0 1) 1
                          (> 0 2) 2
                          (< 0 3) 3))))

      (is (= (list 1 2) (evall ''(1 2))))
      (is (= [1 2] (evall ''(1 2))))

      ; (is (= nil  (evall '(my-cond '(false 1)))))
      ; (is (= 1  (evall '(my-cond '((< 0 1) 1
      ;                                      true 2)))))
      ; (is (= 2  (evall '(my-cond '((> 0 1) 3
      ;                                      true 2)))))
      ; (is (= 3  (evall '(my-cond '((> 0 1) 1
      ;                                      (> 0 2) 2
      ;                                      (< 0 2) 3)))))

      (is (= 1 (evall '(first '(1 3 4)))))
      (is (= 2 (evall '(first '((+ 1 1) 3 4)))))
      (is (= 2 (evall '(if (first '(false 3 4)) 1 2))))
      (is (= 2 (evall '(if (first '((> 1 2) 3 4)) 1 2)))))

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
      (is (= 3 (evall '(if true 3 2))))

      (is (= 3 (evall '(if (and (< 0 1) (< 3 4)) 3 2))))

      (is (= 'x (evall '(def x 1))))

      (is (= 1 (evall '(do
                         (def x 1)
                         x))))
      (is (= 3 (evall '(do
                         (def x 1)
                         (def y 2)
                         (+ x y)))))
      (is (= 4 (evall '(do
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

    (testing "compound-proc"
      ; anonymous
      (is (= 10 (evall '((fn [x] x) 10))))
      (is (= 50 (evall '((fn [x y] (* x y)) 10 5))))

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

      ; wrong and = or   Can't be done now
      ; (is (= true (evall '(do
      ;                       (set! and (fn [a b] (if a a b)))
      ;                       (and true false)))))
      )

    (testing "set get def"

      (is (= 3 (evall '(do (def x 3) x))))
      (is (= 2 (evall '(do (def y (+ 1 1)) y)))))

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
