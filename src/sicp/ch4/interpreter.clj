(ns sicp.ch4.interpreter
  (:require [clojure.test :refer :all]))

(def env (atom {}))
(def eval-f)
(def apply-f)

(def primitive-proc-symbols #{'+ '- '* '/ '< '> '=})

(defn primitive-proc? [exp]
  (primitive-proc-symbols exp))

(defn self-eval? [exp]
  (or
   (number? exp)
   (string? exp)
   (nil? exp)
   (primitive-proc? exp)))

;; TODO: implement
(defn variable? [exp] (#{'x 'y} exp))
(defn get-var-val [exp env] (get @env exp))

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
(defn eval-seq [exp-seq env]
  (if (empty? (next exp-seq))
    (eval-f (first exp-seq) env)
    (recur (next exp-seq) env)
    ))

(defn extend-env [params args env])

(defn if? [exp](= 'if (operator exp)))
(defn if-pred [exp] (second exp))
(defn if-consequent [exp] (nth exp 2))
(defn if-alternative [exp](nth exp 3))

(defn eval-if [exp env]
  (if (true? (eval-f (if-pred exp) env))
    (eval-f (if-consequent exp) env)
    (eval-f (if-alternative exp) env)
    ))

(defn assignment? [exp](= 'set-f! (operator exp)))
(defn assignment-var [exp] (second exp))
(defn assignment-value [exp] (nth exp 2))
(defn set-var-value! [var-name var-val env]
  (swap! env assoc var-name var-val))

(defn eval-assignment [exp env]
  (set-var-value!
    (assignment-var exp)
    (eval-f (assignment-value exp) env)
    env)
  `ok)

(defn definition? [exp](= 'def-f! (operator exp)))
(defn eval-definition [exp env]
  (eval-assignment exp env))

(defn eval-f "string, env {} -> result of application"
  [exp env]
  (cond
    (self-eval? exp) exp
    (variable? exp) (get-var-val exp env)
    (quoteed? exp) (text-of-quot exp)
    (assignment? exp) (eval-assignment exp env)
    (definition? exp) (eval-definition exp env)
    (if? exp) (eval-if exp env)
    (application? exp) (apply-f (eval-f (operator exp) env)
                                (list-of-vals (operands exp) env))

    :else (throw (Exception. (format "Unknown exp type. exp: %s" exp)))))

(defn apply-f [proc args]
  (cond
    (primitive-proc? proc) (apply (eval proc) args)
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

  (testing "eval-assignment"
    (let [env (atom {})]
      (is (= `ok (eval-f '(set-f! x 1) env)))
      (is (= `ok (eval-f '(def-f! y 2) env)))
      (is (= 1 (get @env 'x)))
      (is (= 2 (get @env 'y)))
      (is (= 'x (variable? 'x)))
      (is (= 'y (variable? 'y)))
      (is (= 1 (eval-f 'x env)))
      (is (= 2 (eval-f 'y env)))
      ))

  (testing "Macros"
    (is (= 1 (--> 1)))
    (is (= 5 (--> (+ 1 2)
                  (+ 2))))
    (is (= '(+ 1 2) (to-list '(+ 1 2))))
    )

  (testing "eval-seq"
    (is (= 2 (eval-seq '(1 (+ 1 2) (* 1 2)) {})))
    (is (= nil (eval-seq '() {})))
    )

  (testing "apply-f"
    (is (= '+ (first '(+ 1))))
    (is (primitive-proc? '+))
    (is (primitive-proc? '<))
    (is (not (primitive-proc? +)))
    (is (application? '(+ 1 2)))
    (is (= 3 (eval-f '(+ 1 2) {})))
    (is (= [3 2] (list-of-vals ['(+ 1 2) 2] {})))
    (is (= 3 (eval-f '(- (+  1 2) (* 1 0)) {})))
    (is (= true (eval-f '(< 1 2) {})))
    (is (= false (eval-f '(> 1 2) {})))
    (is (= 3 (eval-f '(if (< 1 3) 3 0) {})))
    (is (= 0 (eval-f '(if (> 1 3) 3 0) {})))
    )

  (testing "list-of-vals"
    ;; order of cons evaluation
    ;; (is (cons (#(prn 0)) [(#(prn 1))]))
    (is (= [1 2] (list-of-vals [1 2] {})))
    (is (= ["foo" 2] (list-of-vals ["foo" 2] {})))
    )

  (testing "eval"
    (is (= 1 (eval-f 1 {})))
    (is (= 1 (eval-f '1 {})))
    (is (= "foo" (eval-f "foo" {})))
    (is (= "foo" (eval-f '"foo" {})))
    (is (not= 1 (eval-f 2 {})))
    (is (thrown? Exception (eval-f :a {}))))
    (is (= '+ (eval-f '+ {})))
  )

(test-interpreter)

(comment
  (eval `(+ 1 2))
  (to-list '(+ 2 3))
  (unquote '2)
  (first (quote (1 2)))
  (first '(1 2))
  (first '(+ 2))
  (eval '+)
  (eval ('+ 1 3))
  (contains? {'x 1} 'x)
  )
