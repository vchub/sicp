(ns sicp.ch4.internal-defs
  (:require [clojure.test :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [sicp.ch4.eval-data-multi :refer :all]))

(defmethod eval-f 'fn [exp env] (list exp env))

(defmethod eval-f :default [exp env] (apply-f
                                       ;; fn
                                      (eval-f (first exp) env)
                                       ;; args
                                      (map #(eval-f % env) (next exp))
                                      env))

(defn merge-envs [e0 e1] (swap! e0 merge @e1) e0)

(defmethod apply-f 'compound-proc [proc-obj args env]
  (let [proc (first proc-obj)
        proc-env (second proc-obj)
        body (proc-body proc)
        env (merge-envs env proc-env)
        env (extend-env env (proc-params proc) args)
        ret (eval-seq body env)]
    ret))

;; (defn env-fixture [f]
;;   (let [env (make-env)
;;         eval-f (fn [exp] (eval-f exp env))
;;         fixture-x 1]
;;     (f)))
;; (use-fixtures :each env-fixture)

(deftest test-internal-defs
  ;; (testing "env-fixture"
  ;;   (is (= 1 fixture-x))
  ;;   (is (= 1 (eval-f '1))))

  (testing "merge-envs infinite loop"
    (let [env (make-env)
          eval-f (fn [exp] (eval-f exp env))]
      (eval-f '(def-f! j 2))
      (eval-f 'j)
      (is (= 2 (eval-f 'j)))

      (eval-f '(def-f! i (fn[x] x)))
      (is (= 'fn (ffirst (eval-f 'i))))
      (is (= 1 (eval-f '(i 1))))
      (is (= 'fn (ffirst (eval-f 'i))))
      (eval-f 'i)
      ;; (is (= 0 (eval-f 'i)))
      ;; (eval-f '(def-f! 'j))
      ))

  ;; TODO: extend-env with fn definition env brings infinite loop
  ;; but this example works in basic variant in sicp.ch4.eval-data-multi
  ;; (testing "ex-4.21"
  ;;   (let [env (make-env)
  ;;         eval-f (fn [exp] (eval-f exp env))
  ;;         factorial (fn [n]
  ;;                     ((fn [f] (f f n))
  ;;                      (fn [f n]
  ;;                        (if (< n 2)
  ;;                          1
  ;;                          (* n (f f (- n 1)))))))
  ;;
  ;;         fib (fn [n]
  ;;               ((fn [f] (f f n))
  ;;                (fn [f n]
  ;;                  (if (< n 2)
  ;;                    1
  ;;                    (+ (f f (- n 1)) (f f (- n 2)))))))]
  ;;
  ;;     (is (= 1 (factorial 1)))
  ;;     (is (= 6 (factorial 3)))
  ;;     (is (= 24 (factorial 4)))
  ;;     (is (= 720 (factorial 6)))
  ;;
  ;;     (is (= 1 (fib 1)))
  ;;     (is (= 2 (fib 2)))
  ;;     (is (= 3 (fib 3)))
  ;;     (is (= 5 (fib 4)))
  ;;     (is (= 8 (fib 5)))
  ;;
  ;;     (eval-f '(def-f! fib (fn [n]
  ;;                            ((fn [f] (f f n))
  ;;                             (fn [f n]
  ;;                               (if (< n 2)
  ;;                                 1
  ;;                                 (+ (f f (- n 1)) (f f (- n 2)))))))))
  ;;
  ;;     (is (= 2 (eval-f '(fib 2))))
  ;;     (is (= 8 (eval-f '(fib 5))))
  ;;
  ;;     (eval-f '(def-f! factorial (fn [n]
  ;;                                  ((fn [f] (f f n))
  ;;                                   (fn [f n]
  ;;                                     (if (< n 2)
  ;;                                       1
  ;;                                       (* n (f f (- n 1)))))))))
  ;;
  ;;     (is (= 24 (eval-f '(factorial 4))))))

  (testing "ex-4.19"
    (let [env (make-env)
          eval-f (fn [exp] (eval-f exp env))]
      (is (= 16 (eval-f '(let [a 1]
                           (def-f! f (fn [x]
                                       (def-f! b (+ a x))
                                       ;; (def-f! b (fn[](+ a x)))
                                       (def-f! a 5)
                                       (+ a b)))
                           (f 10)))))))

  (testing "internal def"
    (let [env (make-env)
          eval-f (fn [exp] (eval-f exp env))]

      ;; (testing "closure"
      ;;   (is (= 1 (eval-f '(do
      ;;                       (def-f! identity (fn [x] x))
      ;;                       (identity 1)))))
      ;;   (eval-f '(do
      ;;              (def-f! x 1)
      ;;              (def-f! one (fn [] x))))
      ;;
      ;;   (is (= 1 (eval-f 'x)))
      ;;   (is (= 1 (eval-f '(one))))
      ;;
      ;;   (eval-f '(do (def-f! make-two (fn []
      ;;                                   (def-f! x 2)
      ;;                                   (fn [] x)))
      ;;                (def-f! two (make-two))))
      ;;   (is (= 2 (eval-f '(two))))
      ;;
      ;;   (testing "mutual recursion"
      ;;     (eval-f
      ;;      '(do
      ;;         (def-f! make-odd-even (fn []
      ;;                                 (def-f! even? (fn [n] (if (= 0 n)
      ;;                                                         true
      ;;                                                         (odd? (- n 1)))))
      ;;                                 (def-f! odd? (fn [n] (if (= 0 n)
      ;;                                                        false
      ;;                                                        (even? (- n 1)))))
      ;;                                 (fn [n] (if (odd? n) (quote odd) (quote even)))))
      ;;         (def-f! odd-even (make-odd-even))
      ;;         (def-f! xxx (make-odd-even))))
      ;;
      ;;     ;; (is (= nil (eval-f 'even?)))
      ;;
      ;;
      ;;     (is (thrown? Exception (eval-f 'odd?)))
      ;;     (is (thrown? Exception (eval-f 'even?)))
      ;;     ;; TODO: stack overflow
      ;;     ;; (is (= nil (eval-f 'make-odd-even)))
      ;;     ;; (is (= nil (eval-f '(def-f! xxx1 (make-odd-even)))))
      ;;
      ;;     (is (= 'even (eval-f '(odd-even 0))))
      ;;     (is (= 'even (eval-f '(odd-even 4))))
      ;;     (is (= 'odd (eval-f '(odd-even 5))))
      ;;     (is (= 'odd (eval-f '(odd-even 7))))))

      (testing "envs"
        (let [e0 (atom {'x 1 'y 2})
              e1 (atom {'x 2})]
          (is (= {'x 2 'y 2} (merge @e0 @e1)))
          (is (= {'x 1 'y 2} @e0))

          (merge-envs e0 e1)
          (is (= {'x 2 'y 2} @e0)))))))

(test-internal-defs)

(comment
  (get {} 1 (throw (Exception. 1))))
