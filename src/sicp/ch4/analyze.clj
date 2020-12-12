(ns sicp.ch4.analyze
  (:require [clojure.test :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [sicp.ch4.eval-data-multi :as s-interp]
            [sicp.ch41.eval-start :as ch41]))

(def eval-f)
(def analyze)
(def apply-f)
(def apply-compound-proc)

(def primitive-proc-symbols #{'+ '- '* '/ '< '> '= 'prn})
(defn make-env [] (atom (into {} (map #(vector % (eval %))) primitive-proc-symbols)))

(defn eval-f [exp env] ((analyze exp env) env))

(defmulti analyze (fn [exp env]
                    (cond
                      (or (number? exp)
                          (string? exp)
                          (boolean? exp)
                          (nil? exp)) 'self-eval
                      (symbol? exp) 'symbol
                      :else (first exp))))

;; TODO: change eval-f to analyze ?


(defn analyze-seq [exp-seq env]
  (let [proc-seq (map #(analyze % env) exp-seq)]
    (fn [env] (reduce (fn [acc f] (f env)) nil proc-seq)))
  ;; (fn [env] (reduce (fn [acc exp] (eval-f exp env)) nil exp-seq))
  )

(defmethod analyze 'self-eval [exp env] (fn [env] exp))
(defmethod analyze 'symbol [exp env] (fn [env] (if-let [ret (get @env exp)]
                                                 ret
                                                 (throw (Exception. (str "Udefined var " exp))))))

(defmethod analyze 'def-f! [exp env] (let [var-name (second exp)
                                           var-proc (analyze (nth exp 2) env)]
                                       (fn [env] (swap! env assoc var-name (var-proc env)))))

(defmethod analyze 'do [exp env] (analyze-seq (next exp) env))

(defmethod analyze 'if [exp env] (fn [env]
                                   (let [pred (analyze (second exp) env)
                                         t-f (analyze (nth exp 2) env)
                                         f-f (analyze (nth exp 3) env)]
                                     (if (pred env) (t-f env) (f-f env)))))

(defn extend-env [env params args]
  (let [p-env (into @env (map vector params args))]
    (atom p-env)))

(defn make-prodedure [params body-proc env]
  (fn [& args]
     ;; (prn params args)
    (let [env (extend-env env params args)]
      (body-proc env))))

(defmethod analyze 'fn [exp env] (fn [env]
                                   (let [params (second exp)
                                         body (drop 2 exp)
                                         body-proc (analyze-seq body env)]
                                       ;; (prn params body)
                                     (make-prodedure params body-proc env))))

(defmethod analyze 'let [exp env]
  (letfn [(iter [pairs]
            (if (empty? pairs)
              (list (list* 'fn [] (drop 2 exp)))
              (let [[p a & pairs] pairs]
                (list (list 'fn [p] (iter pairs)) a))))]
    (let [ret (iter (second exp))]
      ;; (prn ret)
      (analyze ret env)))

  ;; (let [pairs (second exp)
  ;;       body (drop 2 exp)]
  ;;   (if (empty? pairs)
  ;;       ;; (eval-f (list* 'fn [] body) env)
  ;;     (let [exp (list (list* 'fn [] body))]
  ;;       (prn exp)
  ;;       ;; (prn env)
  ;;       ;; (prn (get @env 'x))
  ;;       (eval-f exp env))
  ;;     (let [[p a & pairs] pairs
  ;;           exp (list (list 'fn [p] (list* 'let pairs body)) a)]
  ;;       (prn exp)
  ;;       (eval-f exp env))
  ;;     )
  ;;   )
  )

(defmethod analyze :default [exp env] (fn [env]
                                        (apply-f
                                         ;; fn
                                         (eval-f (first exp) env)
                                         ;; args
                                         (map #(eval-f % env) (next exp))
                                         env)))

(defmulti apply-f (fn [proc args env] (if (seq? proc)
                                        'compound-proc
                                        'primitive-proc)))

(defmethod apply-f 'primitive-proc [proc args env] (apply proc args))

(defmethod apply-f 'compound-proc [proc args env] (apply (eval-f proc) args))

(defn apply-compound-proc [proc args env]
  (apply proc args))

(deftest analyze-test

  (testing "compare analyze and sysntax eval"
    (let [env (make-env)]
      ; (time )
      (let [anal-fact (fn[] (eval-f '(do
                                 (def-f! factorial
                 (fn [n] (if (< n 2) 1 (* n (factorial (- n 1))))))
                                 (factorial 20)) env))
      ch41-fact (fn[](ch41/global-eval '(do
                                   (def factorial
                          (fn [n] (if (< n 2) 1 (* n (factorial (- n 1))))))
                                   (factorial 20))))
      ]
        ; (prn anal-fact)
        ; (prn ch41-fact)
        (is (= (anal-fact) (ch41-fact)))
      ; (time (dotimes [_ 10]
      ;         (anal-fact)))
      ; (time (dotimes [_ 10]
      ;         (ch41-fact)))
        )
      ))

  (testing "let"
    (let [env (make-env)
          eval-f (fn [exp] (eval-f exp env))]
      (is (= 4 (eval-f '((fn [x] ((fn [y] (+ x y)) x)) 2))))
      (is (= 1 (eval-f '((fn [x] ((fn [] x))) 1))))
      (is (= 1 (eval-f '(let [x 1] x))))
      (is (= 8 (eval-f '(let [x 2 y (+ x 2)] (* x y)))))
      ;; (is (= 8 (eval-f '(let [fact (fn[n] (if (< n 2) 1 (* n (fact (- n 1)))))] (fact 3)))))
      ))

  (testing "eval-f primitives"
    (let [env (make-env)
          eval-f (fn [exp] (eval-f exp env))]
      (eval-f '(def-f! one 1))
      (is (= 1 (eval-f 'one)))
      (is (= 1 (eval-f '(do
                          "dome"
                          one))))
      (is (= 3 (eval-f '(+ 1 2))))
      (is (= 6 (eval-f '(+ (/ 4 2) (* 2 2)))))
      (eval-f '(def-f! identity (fn [x] x)))
      (is (= 1 (eval-f '(identity 1))))
      (is (= 4 (eval-f '((fn [x] (* 2 x)) 2))))

      (is (= true (eval-f 'true)))
      (is (= false (eval-f '(< 1 0))))
      (is (= 1 (eval-f '(if true 1 0))))
      (is (= 0 (eval-f '(if (> 0 1) 1 0))))
      (eval-f '(def-f! factorial (fn [n] (if (< n 2) 1 (* n (factorial (- n 1)))))))
      (is (= 1 (eval-f '(factorial 1))))
      (is (= 6 (eval-f '(factorial 3))))))

  (testing "apply closure"
    (let [a [1]
          plus-one (fn [x] (+ x (first a)))]
      (is (= 3 (apply plus-one [2]))))))

(analyze-test)

(comment
  ((fn [x] ((fn [] x))) 1))
