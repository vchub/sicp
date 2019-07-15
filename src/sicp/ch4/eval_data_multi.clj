(ns sicp.ch4.eval-data-multi
  (:require [clojure.test :refer :all]))

(def eval-f)
(def apply-f)

(def primitive-proc-symbols #{'+ '- '* '/ '< '> '=})

(defn make-env [] (atom (-> (into {} (map #(vector % (eval %))) primitive-proc-symbols)
                            ;; (assoc 'true true)
                            ;; (assoc 'false false)
                            )))

(defmulti eval-f (fn [exp env]
                   (cond
                     (or (number? exp) (string? exp) (boolean? exp)) 'self-eval
                     (symbol? exp) 'symbol
                     :else (first exp))))

(defmethod eval-f 'self-eval [exp env] exp)
(defmethod eval-f 'symbol [exp env] (get @env exp))
(defmethod eval-f 'def-f! [exp env] (swap! env  assoc (second exp) (eval-f (nth exp 2) env)))
(defmethod eval-f 'fn [exp env] exp)
(defmethod eval-f 'if [exp env] (if (eval-f (second exp) env)
                                  (eval-f (nth exp 2) env)
                                  (eval-f (nth exp 3) env)))
(defmethod eval-f :default [exp env] (apply-f
                                       ;; fn
                                      (eval-f (first exp) env)
                                       ;; args
                                      (map #(eval-f % env) (next exp))
                                      env))

(defmulti apply-f (fn [proc args env]
                    (cond
                      (seq? proc) 'compound-proc
                      :else 'primitive-proc)))

;; (def compound-proc? seq?)
(defn proc-params [exp] (second exp))
(defn proc-body [exp] (drop 2 exp))
(defn extend-env [env params args]
  (let [p-env (into @env (map vector params args))]
    (atom p-env)))

(defmethod apply-f 'compound-proc [proc args env]
  (let [body (proc-body proc)
        env (extend-env env (proc-params proc) args)
        ret (reduce (fn [acc exp] (eval-f exp env)) nil body)]
    ret))

(defmethod apply-f 'primitive-proc [proc args env] (apply proc args))

;; (defn apply-f
;;   "proc is either '(fn[x y] ....) or primitive-proc"
;;   [proc args env]
;;   (if (compound-proc? proc)
;;     (let [body (proc-body proc)
;;           env (extend-env env (proc-params proc) args)
;;           ret (reduce (fn [acc exp] (eval-f exp env)) nil body)]
;;       ret)
;;     (apply proc args)))

(deftest test-multi-method
  (testing "eval-f"
    (let [env (make-env)
          eval-f (fn [x] (eval-f x env))]
      (is (= 1 (eval-f 1)))
      (is (= "foo" (eval-f "foo")))
      (is (= nil (eval-f 'x)))
      (eval-f '(def-f! x 1))
      ;; (prn env)
      (is (= 1 (eval-f 'x)))
      (is (= '(fn [x y] (+ x y)) (eval-f '(fn [x y] (+ x y)))))
      ;; (is (= '(:apply-f nil (2)) (eval-f '(s 2))))
      (is (= 5 (eval-f '(+ 2 3))))
      (is (= 2/3 (eval-f '(/ 2 3))))

      (is (= true (eval-f 'true)))
      (is (= 2/3 (eval-f '(if true (/ 2 3) (* 1 4)))))
      (is (= 2/3 (eval-f '(if "some" (/ 2 3) (* 1 4)))))
      (is (= 4 (eval-f '(if (< 1 0) (/ 2 3) (* 1 4)))))

      (eval-f '(def-f! sum (fn [x y] (+ x y))))
      ;; (is (= '(:apply-f (fn [x y] (+ x y)) (2 3)) (eval-f '(s 2 3))))
      (is (= 5 (eval-f '(sum 2 3))))
      (is (= 10 (eval-f '(sum (sum 2 3) (sum 2 3)))))
      (is (= 7 (eval-f '(sum (sum 2 3) (* 1 2)))))

      ;; anonymous fn invocation and local def-f!
      (is (= 3 (eval-f '((fn [] 3)))))
      (is (= -1 (eval-f '((fn []
                            (def-f! sum (fn [x y] (- x y)))
                            (* 2 8)
                            (sum 2 3))))))
      ;; in the main scope sum is the same
      (is (= 5 (eval-f '(sum 2 3))))

      ;; recursion
      (eval-f '(def-f! factorial (fn [n] (if (< n 2)
                                           1
                                           (* n (factorial (- n 1)))))))
      (is (= 1 (eval-f '(factorial 1))))
      (is (= 2 (eval-f '(factorial 2))))
      (is (= 6 (eval-f '(factorial 3)))))))

(test-multi-method)

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

(deftest test-data-multi
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

(comment
  ({"some" 1} "some")
  ({"some" 1} :k))
