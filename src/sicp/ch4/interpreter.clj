(ns sicp.ch4.interpreter
  (:require [clojure.test :refer :all]))

(def eval-f)
(def apply-f)

(defn self-eval? [exp]
  (or
   (number? exp)
   (string? exp)
   (#{+ - * /} exp)))

(defn primitive-proc? [exp]
  (#{+ - * /} exp))

(defn variable? [exp])
(defn get-var-val [exp env])

(defn quoteed? [exp])
(defn text-of-quot [exp])

(defn operator [exp] (first exp))
(defn operands [exp] (next exp))
(defn list-of-vals [ops env]
  (if (empty? ops)
    '()
    (cons (eval-f (first ops) env) (list-of-vals (next ops) env))))

(defn application? [exp] (primitive-proc? (operator exp)))

(defn compound-proc? [proc])
(defn proc-body [proc])
(defn proc-params [proc])
(defn proc-env [proc])
(defn eval-seq [exp-seq env])
(defn extend-env [params args env])

(defn eval-f "string, env {} -> result of application"
  [exp env]
  (cond
    (self-eval? exp) exp
    (variable? exp) (get-var-val exp env)
    (quoteed? exp) (text-of-quot exp)
    (application? exp) (apply-f (eval-f (operator exp) env)
                                (list-of-vals (operands exp) env))

    :else (throw (Exception. (format "Unknown exp type. exp: %s" exp)))))

(defn apply-f [proc args]
  (cond
    (primitive-proc? proc) (apply proc args)
    (compound-proc? proc) (eval-seq
                           (proc-body proc)
                           (extend-env
                            (proc-params proc)
                            args
                            (proc-env proc)))
    :else (throw (Exception. (format "Unknown proc type: Apply %s" proc)))))

;; Macros experiments
(defmacro -->
  [exp & forms]
  (if (empty? forms)
    exp
    (let [form (first forms)
          exp (if (seq? form)
                `(~(first form) ~exp ~@(next form))
                (list form exp))]
      `(--> ~exp ~@(next forms)))))

(defmacro to-list [txt]
  `(~@txt))

(deftest test-interpreter
  (testing "Macros"
    (is (= 1 (--> 1)))
    (is (= 5 (--> (+ 1 2)
                  (+ 2))))
    (is (= '(+ 1 2) (to-list '(+ 1 2))))
    )

  (testing "apply-f"
    (is (primitive-proc? +))
    (is (not (primitive-proc? '+)))
    (is (= 3 (eval-f (list + 1 2) {})))
    )

  (testing "list-of-vals"
    ;; order of cons evaluation
    ;; (is (cons (#(prn 0)) [(#(prn 1))]))
    (is (= [1 2] (list-of-vals [1 2] {})))
    (is (= ["foo" 2] (list-of-vals ["foo" 2] {}))))

  (testing "eval"
    (is (= 1 (eval-f 1 {})))
    (is (= 1 (eval-f '1 {})))
    (is (= "foo" (eval-f "foo" {})))
    (is (= "foo" (eval-f '"foo" {})))
    (is (not= 1 (eval-f 2 {})))
    (is (thrown? Exception (eval-f :a {}))))
    (is (= + (eval-f + {})))
  )

(test-interpreter)

(comment
  (eval `(+ 1 2))
  (to-list '(+ 2 3))
  (unquote '2)
  )
