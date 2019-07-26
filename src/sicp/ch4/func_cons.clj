(ns sicp.ch4.func-cons
  (:require [clojure.test :refer :all]))

(def force-it)
(def apply-f)

;; ====================
;; Utils

(defn extend-env [env params args]
  (let [m (java.util.HashMap. (into {} (map vector params args)))]
    (.put m :proto env)
    m))

(defn look-up-map-val [env k]
  "map, key -> [map, value]"
  (if-let [v (get env k)]
    [env v]
    (if-let [m (get env :proto)]
      (recur m k))))

(defn look-up [env k]
  "map, key -> value"
  (if-let [[m v] (look-up-map-val env k)] v))

(defn set-env-var! [env v-name v-val] (.put env v-name v-val) v-val)

(defn memoize-f "memoize no args procedure"
  [proc]
  (let [ret (atom nil)]
    (fn [] (if @ret @ret (do (reset! ret (proc)) @ret)))))

(defn tagged-list [obj tag] (and (seq? obj) (= tag (first obj))))

;; ====================
;; eval
(defmulti eval-f (fn [exp env]
                   (cond
                     (or (number? exp) (string? exp)
                         (boolean? exp)
                         (nil? exp)
                         (and (seq? exp) (empty? exp))) 'self-eval
                     (symbol? exp) 'symbol
                     :else (first exp))))

(defmethod eval-f 'self-eval [exp env] exp)
(defmethod eval-f 'quote [exp env] (second exp))

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

(defmethod eval-f 'symbol [exp env]
  (if-let [ret (look-up env exp)]
    ret
    (throw (Exception. (str "Undefined var ", exp)))))

(defmethod eval-f 'fn [exp env] (list 'compound-proc exp env))

(defmethod eval-f 'def [exp env]
  (let [k (second exp)
        v (eval-f (nth exp 2) env)]
    (set-env-var! env k v)))

(defmethod eval-f 'set! [exp env]
  (let [k (second exp)]
    (if (.containsKey env k)
      (set-env-var! env k (eval-f (nth exp 2) env))
      (throw (Exception. (str "Can't set undefined var " k))))))

(defmethod eval-f 'set-global! [exp env]
  (let [k (second exp)]
    (if-let [[m old-val] (look-up-map-val env k)]
      (set-env-var! m k (eval-f (nth exp 2) env))
      (throw (Exception. (str "Can't set undefined var " k))))))

;; ====================
;; Thunk
(defrecord Thunk [exp env])
(defrecord Realized-Thunk [v])

(defn thunk? [obj] (and (= clojure.lang.Atom (type obj)) (= Thunk (type @obj))))
(defn reaized-thunk? [obj] (and (= clojure.lang.Atom (type obj)) (= Realized-Thunk (type @obj))))

(defn delay-it [exp env] (atom (Thunk. exp env)))

(defn actual-val [exp env] (force-it (eval-f exp env)))

(defn force-it [obj] (cond
                       (reaized-thunk? obj) (:v @obj)
                       (thunk? obj) (let [v (actual-val (:exp @obj) (:env @obj))]
                                      (reset! obj (Realized-Thunk. v))
                                      v)
                       :else obj))

(defmethod eval-f 'delay [exp env] (delay-it (second exp) env))
(defmethod eval-f 'actual-val [exp env] (actual-val (second exp) env))
;; ====================

(defmethod eval-f :default [exp env] (apply-f
                                       ;; fn
                                      ;; (eval-f (first exp) env)
                                      (actual-val (first exp) env)
                                       ;; args
                                      (next exp)
                                      env))

(defmulti apply-f (fn [proc args env]
                    (cond
                      (tagged-list proc 'compound-proc) 'compound-proc
                      :else 'primitive-proc)))

(defmethod apply-f 'compound-proc [proc args env]
  (let [body (drop 2 (second proc))
        proc-env (nth proc 2)
        params (second (second proc))
        ;; args (map #(eval-f % env) args)
        args (map #(delay-it % env) args)
        env (extend-env proc-env params args)
        ret (eval-seq body env)]
    ret))

(defmethod apply-f 'primitive-proc [proc args env]
  ;; (let [args (map #(eval-f % env) args)]
  (let [args (map #(actual-val % env) args)]
    (apply proc args)))

;; ====================
;; env
(def primitive-proc-symbols #{'+ '- '* '/ '< '> '= 'prn})

(defn make-env []
  (let [env (java.util.HashMap. (into {} (map #(vector % (eval %)) primitive-proc-symbols)))]
    (eval-f '(do
               (def cons (fn [x y] (fn [f] (f x y))))
               (def car (fn [z] (z (fn [x y] x))))
               (def cdr (fn [z] (z (fn [x y] y))))) env)
    env))

;; ====================

(deftest test-lazy-cons
  (let [env (make-env)
        eval-f (fn [exp] (eval-f exp env))
        actual-val (fn [obj] (actual-val obj env))]

    (testing "cons"
      (eval-f '(def x (cons 1 2)))
      ;; (is (= 1 (eval-f '(car x))))
      (is (= 1 (actual-val '(car x))))
      (is (= 2 (actual-val '(cdr x))))

      (eval-f '(def x (cons 1 (/ 1 0))))
      (is (= 1 (actual-val '(car x))))
      (is (thrown? ArithmeticException (actual-val '(cdr x))))
      )
    ))

(deftest test-exercises

  (testing "ex 4.30"
    (let [env (make-env)
          eval-f (fn [exp] (eval-f exp env))]
      (eval-f '(def p1 (fn [x] (set! x (cons x '(2))) x)))
      (eval-f '(def y 1))
      (is (= '(2) (actual-val '(cdr (p1 1)) env)))
      (is (not (= 1 (second (actual-val '(car (p1 1)) env)))))
      (is (tagged-list (actual-val '(car (p1 1)) env) 'compound-proc))

      ;;Notice: x is cons - fn
      (is (= '(fn[f] (f x y)) (second (actual-val '(car (p1 1)) env)) ))

      (eval-f '(def p2 (fn [x]
                         (def p (fn [e] e x))
                         (p (set! x (cons x '(2)))))))
      (is (= 1 (actual-val '(p2 1) env)))
      ))

  (testing "ex 4.27"
    (let [env (make-env)
          eval-f (fn [exp] (eval-f exp env))]
      (eval-f '(do
                 (def count 0)
                 (def id (fn [x] (set-global! count (+ count 1)) x))
                 (def w (id (id 10)))))
      (is (= 1 (actual-val 'count env)))
      (is (= 10 (actual-val 'w env)))
      (is (= 2 (actual-val 'count env)))
      (eval-f '(def square (fn [x] (* x x))))
      (is (= 2 (actual-val 'count env)))
      (is (= 100 (eval-f '(square (id 10)))))
      (is (= 3 (actual-val 'count env))))))

(deftest test-internal-fn
  (let [env (make-env)
        eval-f (fn [exp] (eval-f exp env))]
    (eval-f '(def sum (fn [x y] (+ x y))))
    (is (= 3 (eval-f '(sum 1 2))))

    (eval-f '(def adder (fn [z] (fn [y] (sum y z)))))
    (eval-f '(def add-2 (adder 2)))
    (is (= 5 (eval-f '(add-2 3))))
    (eval-f '(do
               (def adder (fn [z] (fn [y] (sum z y))))
               (def add-2 (adder 2))
               (def add-5 (adder 5))))

    (is (= 5 (eval-f '(add-2 3))))
    (is (= 8 (eval-f '(add-5 3))))))

(deftest test-Thunk
  (let [env (make-env)
        eval-f (fn [exp] (eval-f exp env))
        x (Thunk. '(+ 1 2) env)]
    (is (= Thunk (type x)))
    (is (thunk? (delay-it 1 env)))
    (is (= 1 (actual-val 1 env)))
    (is (= 3 (actual-val '(+ 1 2) env)))
    (is (= 3 (actual-val '(delay (+ 1 2)) env)))
    (eval-f '(def x (delay (+ 1 2))))
    (is (= 3 (eval-f '(actual-val x))))
    (is (= 3 (eval-f '(actual-val x))))


    ;; (eval-f '(def sum (fn[x y] (prn "sum" x y) (+ x y))))


    (eval-f '(def sum (fn [x y] (* y x) (+ x y))))
    (eval-f '(def x (sum 3 4)))
    (is (= 7 (eval-f 'x)))
    (is (= 7 (eval-f 'x)))
    (is (= 7 (eval-f 'x)))
    (eval-f '(def z (sum 4 4)))
    (is (= 8 (eval-f 'z)))
    (is (= 8 (eval-f 'z)))))

(deftest test-thunk

  (testing "compound-proc and closure"
    (let [env (make-env)
          eval-f (fn [exp] (eval-f exp env))]
      (eval-f '(def x (+ 1 2)))
      (is (= 3 (eval-f 'x)))
      (eval-f '(def sum (fn [x y] (+ x y))))
      (is (= 3 (eval-f '(sum 1 2))))
      (eval-f '(def dec (fn [x] (- x 1))))
      (eval-f '(def factorial (fn [n] (if (< n 2) 1 (* n (factorial (dec n)))))))
      (is (= 6 (eval-f '(factorial 3))))
      (is (= 2 (eval-f '((fn [x] (sum 1 x)) 1))))

      (eval-f '(do (def f (fn [z] (sum 1 z)))))
      (is (= 4 (eval-f '(f 3))))
      (is (= 4 (eval-f '(def fi (f 3)))))
      (is (= 4 (eval-f 'fi)))

      (eval-f '(def f (fn [z] (fn [] z))))
      (is (= 2 (eval-f '(actual-val ((f 2))))))

      (eval-f '(def adder (fn [z]  (fn [y] (+ y z)))))
      (eval-f '(def add-2 (adder 2)))
      (eval-f '(do
                 (def adder (fn [z] (fn [y] (sum z y))))
                 (def add-2 (adder 2))
                 (def add-5 (adder 5))))

      (is (= 5 (eval-f '(add-2 3))))
      (is (= 8 (eval-f '(add-5 3))))

      (eval-f '(def counter-> (fn []
                                (def c 0)
                                (fn []
                                  (set-global! c (+ 1 c))
                                  c))))
      (eval-f '(def counter (counter->)))
      (is (= 1 (eval-f '(counter))))
      (is (= 2 (eval-f '(counter))))
      (is (= 3 (eval-f '(counter))))))

  (testing "memoize-f"
    (let [x 1 y 2
          f (memoize-f (fn [] (+ x y)))]
      (is (= 3 (f)))))

  (testing "def set!"
    (let [env (make-env)
          eval-f (fn [exp] (eval-f exp env))]
      (eval-f '(def y 2))
      (is (= 2 (eval-f 'y)))
      (is (thrown? Exception (eval-f '(set! x 0))))
      (eval-f '(set! y 4))
      (is (= 4 (eval-f 'y))))))


(deftest test-func-cons
  (test-lazy-cons)
  (test-exercises)
  (test-internal-fn)
  (test-Thunk)
  (test-thunk)
  )

(test-func-cons)
