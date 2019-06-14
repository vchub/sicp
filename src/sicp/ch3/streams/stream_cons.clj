(ns sicp.ch3.streams.stream-cons
  (:require [clojure.test :refer :all]))

(defn mem-proc [proc]
  (let [run? (atom false)
        ret (atom nil)]
    (fn []
      (if @run?
        @ret
        (do (reset! ret (proc)) (reset! run? true) @ret)))))

(def h first)
(defn t [p] (((comp first rest) p)))

(deftest test-stream-basics
  (testing "code stream"
    (let [nums (fn nums [n]
                 [n (mem-proc (fn [] (nums (inc n))))])
          xs (nums 1)]
      (is (= 1 (h xs)))
      (is (= 2 ((comp h t) xs)))
      (is (= 3 ((comp h t t) xs))))

    (let [cnt (atom 0)
          x [1 (constantly nil)]
          y [2 (mem-proc (fn [] (swap! cnt inc) x))]]
      (is (= x (t y)))
      (is (= x (t y)))
      (is (= 1 @cnt))
      (is (= nil (t x)))
      )

    (is (= 2 (t [1 (mem-proc (constantly 2))])))))

(test-stream-basics)

(defprotocol IStream
  (car[_])
  (cdr[_])
  (take-s [s n])
  )

(defrecord Stream [h t]
  IStream
  ;; clojure.lang.ISeq
  ;; (first (fn[] (car s)))
  ;; (next[s] (car s))
  ;; (more[s] (cdr s))
  (car[_] h)
  (cdr[_](t))
  (take-s [s n] (cond
                  (nil? s) nil
                  (< n 1) nil
                  :else
                  (cons (car s) (take-s (cdr s) (dec n)))))
  )

(defn cons-s[h t] (Stream. h (mem-proc t)))

(deftest test-stream
  (testing "code stream"
    (let [p (cons-s 1 (constantly 2))
          nums (fn nums[start] (cons-s start (fn[] (nums (inc start)))))
          xs (nums 2)
          ]
      (is (= 1 (car p)))
      ;; (is (= 1 (next p)))
      (is (= 2 (cdr p)))
      (is (= 2 (car xs)))
      (is (= 3 (car (cdr xs))))
      (is (= '(2) (take-s xs 1)))
      (is (= '(2 3) (take-s xs 2)))
      (is (= '(2 3 4) (take-s xs 3)))
      )
    ))

(test-stream)

(testing "macros"
  (is (= [5] (-> 25
                   (Math/sqrt)
                   (int)
                   (list))))
  )

(comment
  (clojure.repl/apropos "mem")
  (clojure.repl/find-doc "mem")
  (clojure.repl/doc cons)
  )
