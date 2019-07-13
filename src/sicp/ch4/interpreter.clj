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
   ;; TODO: remove nil
   (nil? exp)
   ;; TODO: remove primitive-proc?. they'll be evaluated
   (primitive-proc? exp)))

(defn tagged-list [exp tag] (if (seq? exp)
                              (= (first exp) tag)
                              false))

(def variable? symbol?)
(defn get-var-val [exp env] (get @env exp))

(defn quoted? [exp] (tagged-list exp 'quote))
(defn text-of-quot [exp] (fnext exp))

(defn operator [exp] (first exp))
(defn operands [exp] (next exp))
(defn list-of-vals [ops env]
  (if (empty? ops)
    '()
    (cons (eval-f (first ops) env) (list-of-vals (next ops) env))))

;; (defn application? [exp] (primitive-proc? (operator exp)))
(defn application? [exp] (seq? exp))

(defn compound-proc? [proc])
(defn proc-body [proc])
(defn proc-params [proc])
(defn proc-env [proc])
(defn eval-seq [exp-seq env]
  (if (empty? (next exp-seq))
    (eval-f (first exp-seq) env)
    (recur (next exp-seq) env)))

(defn extend-env [params args env])

(defn if? [exp] (tagged-list exp 'if))
(defn if-pred [exp] (second exp))
(defn if-consequent [exp] (nth exp 2))
(defn if-alternative [exp] (nth exp 3))

(defn make-if [pred t-exp f-exp env]
  (if (true? (eval-f pred env))
    (eval-f t-exp env)
    (eval-f f-exp env)))

(defn eval-if [exp env]
  (make-if (if-pred exp) (if-consequent exp) (if-alternative exp) env))

(defn cond? [exp] (tagged-list exp 'cond))
(defn cond-pairs [exp] (drop 1 exp))

(defn make-cond [cond-pairs env]
  (when (seq cond-pairs)
    (let [pred (first cond-pairs)
          t-exp (second cond-pairs)]
      (make-if pred t-exp (make-cond (drop 2 cond-pairs) env)
            env))))

(defn eval-cond [exp env]
  {:pre (even? (count (cond-pairs exp)))}
  (make-cond (cond-pairs exp) env))

(defn assignment? [exp] (tagged-list exp 'set-f!))
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

(defn definition? [exp] (tagged-list exp 'def-f!))
(defn eval-definition [exp env]
  (eval-assignment exp env))

(defn lambda? [exp] (tagged-list exp 'fn))
(defn lambda-with-name [exp] (symbol? (nth exp 1)))
(defn lambda-params [exp] (if (lambda-with-name exp)
                            (nth exp 2)
                            (nth exp 1)))
(defn lambda-body [exp] (if (lambda-with-name exp)
                          (nth exp 3)
                          (nth exp 2)))
(defn make-lambda
  ([params body]
   (list 'fn params body))
  ([fname params body]
   (list 'fn fname params body)))

(defn make-prodedure
  ([params body env]
   (eval (make-lambda params body)))
  ([fname params body env]
   (eval (make-lambda fname params body))))

(defn do? [exp] (tagged-list exp 'begin))
(defn begin-actions [exp] (next exp))

(defn eval-f "string, env {} -> result of application"
  [exp env]
  (cond
    (self-eval? exp) exp
    (variable? exp) (get-var-val exp env)
    (quoted? exp) (text-of-quot exp)
    (assignment? exp) (eval-assignment exp env)
    (definition? exp) (eval-definition exp env)
    (if? exp) (eval-if exp env)
    (cond? exp) (eval-cond exp env)
    (lambda? exp) (make-prodedure (lambda-params exp) (lambda-body exp) env)
    (do? exp) (eval-seq (begin-actions exp) env)
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
    ;; TODO: remove.  for tests now
    (fn? proc) (apply proc args)

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

  (testing "lambda?"
    (is (lambda? '(fn [x])))
    (is (lambda? '(fn [x] (+ 1 x))))
    (is (lambda? '(fn fx [x] (+ 1 x))))
    (is (=  '(fn [x] (+ 1 x)) (make-lambda '[x] '(+ 1 x))))
    (let [exp1 '(fn [x] (+ 1 x))
          exp2 '(fn f-x [x y] (+ 1 x))]
      (is (= '[x]  (lambda-params exp1)))
      (is (= '[x y]  (lambda-params exp2))))

    (is (=  '(fn f-x [x y] (do (+ y x) (+ x y)))
            (make-lambda 'f-x '[x y] '(do (+ y x) (+ x y)))))

    (let [env (atom {})]
      (is (fn? (eval-f '(fn [x] (+ 1 x)) env)))
      (is (fn? (eval-f '(fn f-x [x] (+ 1 x)) env)))
      (is (fn? (eval-f '(fn f-x [x] (+ 1 x)) env)))
      (is (= `ok (eval-f '(def-f! ff (fn f-x [x] (+ 1 x)) 1) env)))
      (is (fn?  (get-var-val 'ff env)))
      (is (= 2  ((get-var-val 'ff env) 1)))
      (is (= 2 (eval-f '(ff 1) env)))))

  (testing "expressions"
    (is (tagged-list '(t 1 2) 't))
    (is (tagged-list '(quote 1 2) 'quote))
    (is (quoted? '(quote (1 2))))
    (is (quoted? ''(1 2)))
    (is (= '(1 2) (text-of-quot ''(1 2))))
    (is (= '(1 2) (text-of-quot '(quote (1 2)))))

    (is (= '(quote (1 2)) ''(1 2)))
    (is (= (quote (quote (1 2))) ''(1 2)))
    (is (= 'quote (first ''(1 2))))
    (is (symbol? 'x))
    (is (not (symbol? ':b)))
    (is (not (symbol? :b))))

  (testing "eval-assignment"
    (let [env (atom {})]
      (is (= `ok (eval-f '(set-f! x 1) env)))
      (is (= `ok (eval-f '(def-f! y 2) env)))
      (is (= 1 (get @env 'x)))
      (is (= 2 (get @env 'y)))
      (is (variable? 'x))
      (is (variable? 'xb))
      (is (= 1 (eval-f 'x env)))
      (is (= 2 (eval-f 'y env)))
      (is (= 3 (eval-f '(+ x y) env)))
      (is (= `ok (eval-f '(set-f! x (/ 4 2)) env)))
      (is (= 4 (eval-f '(+ x y) env)))
      (is (= `ok (eval-f '(def-f! z (* x y)) env)))
      (is (= 8 (eval-f '(+ x y z) env)))))

  (testing "Macros"
    (is (= '(+ 1 2) (to-list '(+ 1 2))))
    (is (= 1 (--> 1)))
    (is (= 6 (--> (+ 1 2)
                  (+ 2)
                  inc)))
    (is (= 6 (-> (+ 1 2)
                 (+ 2)
                 inc))))

  (testing "eval-seq"
    (is (= 2 (eval-seq '(1 (+ 1 2) (* 1 2)) {})))
    (is (= nil (eval-seq '() {}))))

  (testing "if and cond"
    (is (= true (eval-f '(< 1 2) {})))
    (is (= false (eval-f '(> 1 2) {})))
    (is (= 3 (eval-f '(if (< 1 3) 3 0) {})))
    ;; (is (= 3 (eval-f '(if false 3 0) {})))
    (is (= 0 (eval-f '(if (> 1 3) 3 0) {})))
    (is (= 0 (eval-f '(if nil 3 0) {})))
    (is (= 2 (eval-f '(if (> 1 3) 3 (* 1 2)) {})))
    (is (= 2 (eval-f '(cond
                        (> 1 3) 3
                        (< 1 3) (* 1 2)) {})))
    (is (= 6 (eval-f '(cond
                        (> 1 3) 1
                        (> 2 3) 2
                        (= 0 0) (if (< 1 2) (* 2 3) (* 1 0))) {}))))

  (testing "apply-f"
    (is (= '+ (first '(+ 1))))
    (is (primitive-proc? '+))
    (is (primitive-proc? '<))
    (is (not (primitive-proc? +)))
    (is (application? '(+ 1 2)))
    (is (= 3 (eval-f '(+ 1 2) {})))
    (is (= [3 2] (list-of-vals ['(+ 1 2) 2] {})))
    (is (= 3 (eval-f '(- (+  1 2) (* 1 0)) {}))))

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
  (is (= '+ (eval-f '+ {}))))

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
  '(1 1)
  ''(1 1)
  (1 1)
  (vec '[x])
  (fn [x])
  (let [p 'x
        ;; b '(+ x 1)
        b '(+ p 1)
        ;; f (fn (vec p))
        ;; f (fn (vector p))
        ;; f (fn [p] (+ p 1))
        f (fn [p] b)]
    (f 2))
  (read-string "(if true :t :f)")
  (eval (read-string "(if true :t :f)"))
  (eval "(if true :t :f)")
  (eval '(if true :t :f))
  (list* 1 [2] [3]))
