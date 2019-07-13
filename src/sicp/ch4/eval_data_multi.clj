(ns sicp.ch4.eval-data-multi
  (:require [clojure.test :refer :all]))

(def eval-f)
(def apply-f)

(def primitive-proc-symbols #{'+ '- '* '/ '< '> '=})

(defprotocol Pair
  (h [_] "head")
  (t [_] "tail"))

(extend clojure.lang.ISeq
  Pair
  {:h (fn [s] (first s))
   :t (fn [s] (next s))})

(extend clojure.lang.Symbol
  Pair
  {:h (fn [s] 'symbol)
   :t (fn [s] nil)})

(extend java.lang.String
  Pair
  {:h (fn [s] 'string)})

(defmulti eval-f (fn [exp]
                   (cond
                     (or (number? exp) (string? exp)) 'self-eval
                     (symbol? exp) 'symbol
                     :else (first exp))))

(defmethod eval-f 'self-eval [s] s)
;; TODO: fix to var eval
(defmethod eval-f 'symbol [s] s)

(deftest test-data-multi
  (testing "eval-f"
    (is (= 1 (eval-f 1)))
    (is (= "foo" (eval-f "foo")))
    (is (= 'x (eval-f 'x))))

  (testing "protocol"
    (is (= 1 ({"foo" 1} "foo")))
    ;; (is (= clojure.lang.ISeq (class '())))
    ;; (is (= clojure.lang.ISeq (class '(foo bar))))
    ;; (is (= clojure.lang.ISeq (class [])))
    (is (= clojure.lang.Symbol (class 'x)))
    (is (= java.lang.String (class "some")))
    ;; (is (= clojure.lang.IFn (class (fn[x]))))
    (is (= 1 (h '(1 2 3))))
    (is (= '(2 3) (t '(1 2 3))))
    (is (= 'string (h "foo")))
    ;; (is (= 'symbol (h 'x)))
    ;; (is (= nil (t 'x)))
    ))

(test-data-multi)
