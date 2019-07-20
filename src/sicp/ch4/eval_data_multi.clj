(ns sicp.ch4.eval-data-multi
  (:require [clojure.test :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]))

(def eval-f)
(def apply-f)

(def primitive-proc-symbols #{'+ '- '* '/ '< '> '= 'prn})

(defn make-env [] (atom (-> (into {} (map #(vector % (eval %))) primitive-proc-symbols)
                            ;; (assoc 'true true)
                            ;; (assoc 'false false)
                            )))

(defmulti eval-f (fn [exp env]
                   (cond
                     (or (number? exp) (string? exp)
                         (boolean? exp)
                         (nil? exp)
                         (and (seq? exp) (empty? exp))) 'self-eval
                     (symbol? exp) 'symbol
                     :else (first exp))))

(defmethod eval-f 'self-eval [exp env] exp)
(defmethod eval-f 'symbol [exp env] (if-let [ret (get @env exp)]
                                      ret
                                      (throw (Exception. (str "Unbound var ", exp)))))

(defmethod eval-f 'quote [exp env] (second exp))
(defmethod eval-f 'def-f! [exp env]
  (let [v (eval-f (nth exp 2) env)] (swap! env  assoc (second exp) v) v))

(defmethod eval-f 'fn [exp env] exp)
(defmethod eval-f 'if [exp env] (if (eval-f (second exp) env)
                                  (eval-f (nth exp 2) env)
                                  (eval-f (nth exp 3) env)))

(defmethod eval-f 'and [exp env] (loop [acc (eval-f (fnext exp) env) exp (nnext exp)]
                                   (cond
                                     (empty? exp) acc
                                     (not acc) false
                                     :else (recur (eval-f (first exp) env) (next exp)))))

(defmethod eval-f 'not [exp env] (not (eval-f (second exp) env)))

(defn split-for-let [xs] (loop [ps [] as [] xs xs]
                           (if (empty? xs)
                             [ps as]
                             (let [[p a & xs] xs]
                               (recur (conj ps p) (conj as a) xs)))))

(defmethod eval-f 'let-seq [exp env] (let [body (drop 2 exp)
                                           [params args] (split-for-let (second exp))
                                           f (list* 'fn params body)]
                                       (eval-f (list* f args) env)))

(defn eval-seq [exp-seq env]
  (reduce (fn [acc exp] (eval-f exp env)) nil exp-seq))

(defmethod eval-f 'let [exp env] (let [body (drop 2 exp)
                                       pairs (second exp)]
                                   (if (empty? pairs)
                                     (eval-seq body env)
                                     (let [[param arg & pairs] pairs
                                           f (list 'fn [param] (list* 'let pairs body))]
                                       (eval-f (list f arg) env)))))

(defmethod eval-f 'do [exp env] (eval-seq (next exp) env))

(defmethod eval-f 'loop [exp env] (let [body (drop 2 exp)
                                        pairs (second exp)
                                        [params args] (split-for-let pairs)
                                        recur (list* 'fn params body)
                                        pairs (list* 'recur recur pairs)
                                        f (list 'let pairs (list* 'recur params))]
                                    (eval-f f env)))

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
        ret (eval-seq body env)]
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

(defn repl- []
  (let [env (make-env)
        prompt (symbol "# ")]
    (loop []
      (print prompt)
      (flush)
      (let [line (read-string (read-line))]
        (if (= line 'C)
          'ok
          (do
            (prn line)
            (prn (eval-f line env))
          ;; (Thread/sleep 100)
            (recur)))))))

(deftest test-multi-method

  (testing "ex-4.21"
    (let [env (make-env)
          eval-f (fn [exp] (eval-f exp env))
          even? (fn [n]
                  ((fn [even? odd?] (even? even? odd? n))
                   (fn [even? odd? n]
                     (if (= 0 n)
                       true
                       (odd? even? odd? (dec n))))
                   (fn [even? odd? n]
                     (if (= 0 n)
                       false
                       (even? even? odd? (dec n))))
                   )
                  )]

      (eval-f '(def-f! fib (fn [n]
                             ((fn [f] (f f n))
                              (fn [f n]
                                (if (< n 2)
                                  1
                                  (+ (f f (- n 1)) (f f (- n 2)))))))))
      (is (= 2 (eval-f '(fib 2))))
      (is (= 8 (eval-f '(fib 5))))

      (is (even? 2))
      (is (even? 4))
      (is (not (even? 5)))

      (eval-f '(def-f! even? (fn [n]
                  ((fn [even? odd?] (even? even? odd? n))
                   (fn [even? odd? n]
                     (if (= 0 n)
                       true
                       (odd? even? odd? (- n 1))))
                   (fn [even? odd? n]
                     (if (= 0 n)
                       false
                       (even? even? odd? (- n 1))))
                   )
                  )))
      (is (eval-f '(even? 4)))
      (is (not (eval-f '(even? 5))))
      ))

  (testing "eval-f"
    (let [env (make-env)
          eval-f (fn [x] (eval-f x env))]

      (testing "mutual recursion"
        (is (= 2 (eval-f '(do 1 2))))
        (is (list?
             (eval-f
              '(def-f! odd-even (fn [n]
                                     ;; (def-f! odd? nil)
                                  (def-f! even? (fn [n] (if (= 0 n)
                                                          true
                                                          (odd? (- n 1)))))
                                  (def-f! odd? (fn [n] (if (= 0 n)
                                                         false
                                                         (even? (- n 1)))))
                                     ;; (prn "in odd-even" (even? 0))
                                     ;; (prn even?)
                                     ;; (prn "in odd-even" (odd? 0))
                                     ;; (prn odd?)
                                  (if (odd? n) (quote odd) (quote even)))))))

        (defspec even-spec 10
          (prop/for-all [n (->> gen/nat
                                (gen/such-that even?))]
                        (let [ret (eval-f (list 'odd-even n))]
                          (= 'even ret))))
        (defspec odd-spec 10
          (prop/for-all [n (->> gen/nat
                                (gen/such-that odd?))]
                        (let [ret (eval-f (list 'odd-even n))]
                          (= 'odd ret))))
        ;; (even-spec)
        ;; (odd-spec)

        (is (= 'even (eval-f '(odd-even 0))))
        (is (= 'even (eval-f '(odd-even 4))))
        (is (= 'odd (eval-f '(odd-even 5))))

        (is (list? (eval-f 'odd-even)))
        (is (= 'fn (first (eval-f 'odd-even)))))

      (testing "prn"
        (is (= 1 (eval-f '((fn []
                             ;; (prn "test")
                             ;; (prn (< 1 0))
                             1))))))

      (testing "and not"
        (is (= 3 (and 0 1 2 3)))
        (is (= false (and 0 (< 1 0) 1 2 3)))
        (is (= 3 (eval-f '(and 0 1 2 3))))
        (is (= false (eval-f '(and 0 1 (< 1 0) 2 (< 0 1)))))
        (is (= false (eval-f '(not 1))))
        (is (= true (eval-f '(not false))))
        (is (= true (eval-f '(not (= 1 2))))))

      (testing "truthness"
        (is (= true (eval-f 'true)))
        (is (= false (eval-f 'false)))
        (is (= nil (eval-f 'nil)))
        (is (= '() (eval-f '()))))

      (testing "loop"
        (is (= 15 (loop [n 5 acc (- n 5)]
                    (if (neg? n)
                      acc
                      (recur (dec n) (+ n acc))))))

        (is (= 15 (eval-f '(let [recur (fn [n acc]
                                         (if (< n 0)
                                           acc
                                           (recur (- n 1) (+ n acc))))]
                             (recur 5 0)))))

        (is (= 15 (eval-f '(loop [n 5 acc (- n 5)]
                             (if (< n 0)
                               acc
                               (recur (- n 1) (+ n acc))))))))

      (testing "let"
        (is (= 1 (eval-f '(let [x 1] x))))
        (is (= 36 (eval-f '[let [x (* 2 3)] (+ 2 2) (* x x)])))
        (is (= 6 (eval-f '(let [x 2 y 3] (* x y)))))
        (is (= 4 (eval-f '(let [x 2 y x] (* x y)))))
        (is (= 12 (eval-f '(let [f (fn [x] x) double-f (fn [x] (+ (f x) (f x)))]
                             (* (f 2) (double-f 3))))))
        (is (= 6 (eval-f '(let [factorial (fn [n]
                                            (if (< n 2)
                                              1
                                              (* n (factorial (- n 1)))))]
                            (factorial 3))))))

      (testing "let-seq"
        (is (= '([x y] [1 2]) (split-for-let '[x 1 y 2])))
        (is (= '([x] [1]) (split-for-let '[x 1])))
        (is (= 1 (eval-f '(let-seq [x 1] x))))
        (is (= 4 (eval-f '(let-seq [x 2] (* x x)))))
        (is (= 6 (eval-f '(let-seq [x 2 y 3] (+ 2 2) (* x y)))))
        (is (thrown? Exception (eval-f '(let-seq [x 2 y x] (* x y))))))

      (is (= 1 (eval-f 1)))
      (is (= "foo" (eval-f "foo")))
      (is (thrown? Exception (eval-f 'x)))
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
      (is (= 2/3 (eval-f '(if () (/ 2 3) (* 1 4)))))
      (is (= 4 (eval-f '(if nil (/ 2 3) (* 1 4)))))
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
  ({"some" 1} :k)
  (let [] 1))
