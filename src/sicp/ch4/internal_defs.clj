(ns sicp.ch4.internal-defs
  (:require [clojure.test :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [sicp.ch4.eval-data-multi :refer :all]))

(defmethod eval-f 'fn [exp env] (list exp env))

(defn merge-envs [e0 e1] (swap! e0 merge @e1) e0)

(defmethod apply-f 'compound-proc [proc-obj args env]
  (let [proc (first proc-obj)
        proc-env (second proc-obj)
        ;; _ (prn proc-env)
        body (proc-body proc)
        env (merge-envs env proc-env)
        env (extend-env env (proc-params proc) args)
        ret (eval-seq body env)]
    ret))

(deftest test-internal-defs
  (testing "internal def"
    (let [env (make-env)
          eval-f (fn [exp] (eval-f exp env))]
      (testing "closure"
        (is (= 1 (eval-f '(do
                            (def-f! identity (fn [x] x))
                            (identity 1)))))
        (eval-f '(do
                   (def-f! x 1)
                   (def-f! one (fn [] x))))

        (is (= 1 (eval-f 'x)))
        (is (= 1 (eval-f '(one))))

        (eval-f '(do (def-f! make-two (fn []
                                        (def-f! x 2)
                                        (fn [] x)))
                     (def-f! two (make-two))))
        (is (= 2 (eval-f '(two))))

        (testing "mutual recursion"
          (eval-f
           '(do
              (def-f! make-odd-even (fn []
                                      (def-f! even? (fn [n] (if (= 0 n)
                                                              true
                                                              (odd? (- n 1)))))
                                      (def-f! odd? (fn [n] (if (= 0 n)
                                                             false
                                                             (even? (- n 1)))))
                                      (fn [n] (if (odd? n) (quote odd) (quote even)))))
              (def-f! odd-even (make-odd-even))
              (def-f! xxx (make-odd-even))i
              ))

          (is (= nil (eval-f 'odd?)))
          (is (= nil (eval-f 'even?)))
          ;; TODO: stack overflow
          ;; (is (= nil (eval-f '(do 1
          ;;                                     (def-f! xx (make-odd-even))))))

          (is (= 'even (eval-f '(odd-even 0))))
          (is (= 'even (eval-f '(odd-even 4))))
          (is (= 'odd (eval-f '(odd-even 5))))
          (is (= 'odd (eval-f '(odd-even 7))))))

      (testing "envs"
        (let [e0 (atom {'x 1 'y 2})
              e1 (atom {'x 2})]
          (is (= {'x 2 'y 2} (merge @e0 @e1)))
          (is (= {'x 1 'y 2} @e0))

          (merge-envs e0 e1)
          (is (= {'x 2 'y 2} @e0)))))))

(test-internal-defs)
