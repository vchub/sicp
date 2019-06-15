(ns sicp.ch3.streams.macros
  (:require [clojure.test :refer :all])
  )

(defn unless-fn [test-e exp-fn]
  (if test-e nil (exp-fn)))

(defn unless-fn-1 [test-e & exps]
  (if test-e nil (last exps)))

(defmacro unless-m [test & body]
  ;; (list 'if (list 'not test) (cons 'do body))
  (list 'if test nil (cons 'do body))
  )

(defmacro unless-m-1 [test body]
  ;; (list 'if (list 'not test) (cons 'do body))
  (list 'if test nil body)
  )

(deftest test-unless

(testing "unless-m-1"
  ;; (prn (macroexpand '(unless-m (< 0 1) 3 2 1)))
    (is (= nil (unless-m-1 (< 0 1) (do 3 2 1))))
    (is (= 1 (unless-m-1 (> 0 1) (do 3 2 1))))
    )

(testing "unless-m"
  ;; (prn (macroexpand '(unless-m (< 0 1) 3 2 1)))
    (is (= nil (unless-m (< 0 1) 3 2 1)))
    (is (= 1 (unless-m (> 0 1) 3 2 1)))
    )

(testing "unless-fn-1"
    (is (= nil (unless-fn-1 (< 0 1) 3 2 1)))
    (is (= 1 (unless-fn-1 (> 0 1) 3 2 1)))
    )
  (testing "unless-fn"
    (is (= nil (unless-fn (< 0 1) (fn[] 2 3 1))))
    (is (= 1 (unless-fn (> 0 1) (fn[] 2 3 1))))
    )
  )

(test-unless)
