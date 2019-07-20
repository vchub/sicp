(ns sicp.ch4.env-continue
  (:require [clojure.test :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [sicp.ch4.eval-data-multi :refer :all]))

(defmethod eval-f 'fn [exp env] (list exp env))
;; (defmethod eval-f 'fn [exp env] exp)

(defmethod eval-f :default [exp env] (apply-f
                                       ;; fn
                                      (eval-f (first exp) env)
                                       ;; args
                                      (map #(eval-f % env) (next exp))
                                      env))

(defn merge-envs [e0 e1] (atom @e0 merge @e1))

(defmethod apply-f 'compound-proc [proc-obj args env]
  (let [
        proc (first proc-obj)
        proc-env (second proc-obj)
        ;; proc proc-obj
        ;; proc-env (second proc-obj)
        body (proc-body proc)
        ;; env (merge-envs env proc-env)
        env (extend-env env (proc-params proc) args)
        ret (eval-seq body env)]
    ret))


(deftest test-env

  (testing "merge-envs infinite loop"
    (let [
          ;; env (make-env)
          env (atom {})
          eval-f (fn [exp] (eval-f exp env))]

      (eval-f '(def-f! make-identity (fn[] (fn [n] n))))
      ;; (eval-f '(def-f! identity (make-identity)))
      ;; (is (= 4 (eval-f '(identity 4))))
      ;; (is (= 8 (eval-f '((make-identity) 8))))
      ;; (eval-f '(make-identity))
      ;; (is (= '((fn []...) {}) (eval-f '(make-identity))))
      ;; (is (= '(fn [n] n) (first (eval-f '(make-identity)))))

      ;; (prn env)
      ;; (is (= '{} (second (eval-f '(make-identity)))))

      ))




  )

(test-env)

(comment
  (get {} 1 (throw (Exception. 1))))
