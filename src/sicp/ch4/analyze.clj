(ns sicp.ch4.analyze
  (:require [clojure.test :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]))

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
  (fn [env] (reduce (fn [acc exp] (eval-f exp env)) nil exp-seq)))

(defmethod analyze 'self-eval [exp env] (fn [env] exp))
(defmethod analyze 'symbol [exp env] (fn [env] (if-let [ret (get @env exp)]
                                                 ret
                                                 (throw (Exception. (str "Udefined var " exp))))))

(defmethod analyze 'def-f! [exp env] (let [var-name (second exp)
                                           var-val (eval-f (nth exp 2) env)]
                                       (fn [env] (swap! env assoc var-name var-val))))

(defmethod analyze 'do [exp env] (analyze-seq (next exp) env))

(defmethod analyze 'if [exp env] (fn [env]
                                   (let [pred (analyze (second exp) env)
                                         t-f (analyze (nth exp 2) env)
                                         f-f (analyze (nth exp 3) env)]
                                     (if (pred env) (t-f env) (f-f env)))))

(defn extend-env [env params args]
  (let [p-env (into @env (map vector params args))]
    (atom p-env)))

(defmethod analyze 'fn [exp env] (fn [env]
                                   (let [params (second exp)
                                         body (drop 2 exp)
                                         body-proc (analyze-seq body env)]
                                       ;; (prn params body)
                                     (fn [& args]
                                       ;; (prn params body args)
                                       ;; (prn body-proc)
                                       (let [env (extend-env env params args)]
                                         (body-proc env))))))

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
