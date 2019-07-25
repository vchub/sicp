(ns sicp.ch4.thunk
  (:require [clojure.test :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [sicp.ch4.eval-data-multi :refer [eval-f]]))

(def force-it)
(def apply-f)

(def primitive-proc-symbols #{'+ '- '* '/ '< '> '= 'prn})

(defn make-env []
  (java.util.HashMap. (into {} (map #(vector % (eval %)) primitive-proc-symbols))))

(defn look-up-map-val [env k]
  "map, key -> [map, value]"
  (if-let [v (get env k)]
    [env v]
    (if-let [m (get env :proto)]
      (recur m k))))

(defn look-up [env k]
  "map, key -> value"
  (if-let [[m v] (look-up-map-val env k)] v))

(defmethod eval-f 'symbol [exp env]
  (if-let [ret (look-up env exp)]
    ret
    (throw (Exception. (str "Undefined var ", exp)))))

(defn set-env-var! [env v-name v-val] (.put env v-name v-val) v-val)

(defn memoize-f "memoize no args procedure"
  [proc]
  (let [ret (atom nil)]
    (fn [] (if @ret @ret (do (reset! ret (proc)) @ret)))))

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

(defn tagged-list [obj tag] (and (seq? obj) (= tag (first obj))))
(defn delay-it [exp env] (list 'thunk exp env))
(defn thunk? [obj] (tagged-list obj 'thunk))
(defn reaized-thunk? [obj] (tagged-list obj 'thunk))

(defn actual-val [exp env] (force-it (eval-f exp env)))

(defn force-it [obj] (if (thunk? obj)
                       (actual-val (second obj) (nth obj 2))
                       obj))

;; (defn delayed? [exp] (and (seq? exp)
;;                           (= 'delayed (first exp))))
;;
;; (defn force-f [exp]  (if (delayed? exp)
;;                        (if (delayed? (second exp))
;;                          (recur (second exp))
;;                          ((second exp)))
;;                        exp))
;;
;; (defn delay-> [exp env]  (list 'delayed (memoize-f (fn [] (eval-f exp env)))))
;;
(defmethod eval-f 'thunk [exp env] exp)
(defmethod eval-f 'delay [exp env] (delay-it exp env))
(defmethod eval-f 'force [exp env] (force-it exp env))

(defmethod eval-f :default [exp env] (apply-f
                                       ;; fn
                                      ;; (eval-f (first exp) env)
                                      (actual-val (first exp) env)
                                       ;; args
                                      ;; (map #(eval-f % env) (next exp))
                                      (next exp)
                                      env))

(defmulti apply-f (fn [proc args env]
                    (cond
                      (tagged-list proc 'compound-proc) 'compound-proc
                      :else 'primitive-proc)))

(defn extend-env [env params args]
  (let [m (java.util.HashMap. (into {} (map vector params args)))]
    (.put m :proto env)
    m))

(defn eval-seq [exp-seq env]
  (reduce (fn [acc exp] (eval-f exp env)) nil exp-seq))

(defmethod apply-f 'compound-proc [proc args env]
  (let [body (drop 2 (second proc))
        proc-env (nth proc 2)
        params (second (second proc))
        ;; TODO: change to delay
        args (map #(eval-f % env) args)
        env (extend-env proc-env params args)
        ret (eval-seq body env)]
    ret))

(defmethod apply-f 'primitive-proc [proc args env]
  (let [args (map #(actual-val % env) args)]
    (apply proc args)))

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

(deftest test-thunk

  (test-internal-fn)

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
      ;;
      ;; (eval-f '(def f (fn [z] (sum 1 0))))
      (eval-f '(do (def f (fn [z] (sum 1 z)))))
      (is (= 4 (eval-f '(f 3))))
      (is (= 4 (eval-f '(def fi (f 3)))))
      (is (= 4 (eval-f 'fi)))

      (eval-f '(def f (fn [z] (fn [] z))))
      (is (= 2 (eval-f '((f 2)))))

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

  (testing "proc args lazy eval"
    (let [env (make-env)
          eval-f (fn [exp] (eval-f exp env))]
      ;; (is (= 2 (eval-f '(+ 1 1))))
      ;; (is (= 2 (eval-f '(force 2))))
      ;; (is (= 2 (eval-f '(force (+ 1 1)))))
      ;; (is (= 'delayed (first (eval-f '(delay (+ 1 1))))))
      ;; (is (= 2 ((second (eval-f '(delay (+ 1 1)))))))
      ;; (is (= 2 (eval-f '(force (delay (+ 1 1))))))
      ;; (is (= 9 (eval-f '(do
      ;;                     (def x-x 4)
      ;;                     (def x-d (delay (+ x-x 5)))
      ;;                     (force x-d)))))
      ;;
      ;; (is (= 2 (eval-f '(+ 1 (delay 1)))))
      ;; (is (= 2 (eval-f '(do
      ;;                     (def x (delay 1))
      ;;                     (+ x 1)))))
      ;;
      ;; (is (= 2 (eval-f '(force (delay (delay (+ 1 1)))))))
      ;;
      ;; (eval-f '(def ident (fn [x]  x)))
      ;; (is (delayed? (eval-f '(ident 1))))
      ;; ;; (is (= 10 (eval-f '(force (ident 10)))))
      ;; (is (= 3 (eval-f '(+ (ident 2) (ident 1)))))
      ;; (is (= 3 (eval-f '(+ (ident 2) (ident 1)))))
      ;; ;; (eval-f '(def f-ident (fn [x] (prn "in f-ident" x) x)))
      ;; ;; (is (delayed? (eval-f '(f-ident (ident 3)))))
      ;; ;;
      ;; (eval-f '(def sum (fn [x y] (+ x y))))
      ;; (is (delayed? (eval-f '(sum (ident 20) (ident 11)))))
      ;; (is (= 31 (eval-f '(+ 0 (sum (ident 20) (ident 11))))))
      ;;
      ;; (is (= 31 (eval-f '(do (+ 0 (sum (ident 20) (ident 11)))))))

      ;; (is (= 2 (eval-f '(force (force (fn[]
      ;;                     (def inc- (fn [x] (+ 1 x)))
      ;;                     (def i-f (fn [x]  x))
      ;;                     (+ 0 (i-f (inc- 1)))))
      ;;                     ))))

      ;; TODO: fix-it
      ;; (is (= 2 (eval-f '(+ 1 (delay (delay 1))))))

      ;; (is (= 2 (eval-f '(do
      ;;                     (def inc- (fn [x] (+ 1 x)))
      ;;                     (inc- 1)
      ;;                     (+ 0 (inc- 1))
      ;;                     (def f (fn [x] (inc- x)))
      ;;                     (+ 0 (force (force (f 1))))
      ;;                     (def i-f (fn [x] (prn x) x))
      ;;                     (+ 0 (force (i-f (inc- 1))))))))
      ))

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

(test-thunk)

(deftest java-map
  (testing "eval-f"
    (let [env (make-env)
          env2 (extend-env env '[x y] [1 2])
          eval-f (fn [exp] (eval-f exp env))]
      (eval-f '(def y 2))
      (is (= 2 (eval-f 'y)))
      (is (= 3 (eval-f '(+ 1 y))))))

  (testing "make-env"
    (let [m (java.util.HashMap. {:a 1})
          m2 (java.util.HashMap. {:a 0 :proto m})
          env (make-env)
          env2 (extend-env env '[x y] [1 2])
          env3 (extend-env env2 '[a] [:a])]

      (is (= 1 (:a m)))
      (is (= 1 (.put m :a 2)))
      (is (= 2 (:a m)))
      (.replace m :b 2)
      (is (= nil (:b m)))
      (is (= nil (.put m 'b 2)))
      (is (= 2 (.put m 'b 3)))
      (is (= 3 (.get m 'b)))
      (is (= 3 ('b m)))
      (is (= 0 (:a m2)))

      (is (= nil ('b m2)))
      (is (= 0 (look-up m2 :a)))
      (is (= 3 (look-up m2 'b)))
      (set-env-var! m 'c 5)
      (is (= 5 (look-up m2 'c)))

      (is (= + (look-up env '+)))
      (is (= < (look-up env '<)))
      (is (= 1 (look-up env2 'x)))
      (is (= 2 (look-up env2 'y)))
      (is (= nil (look-up env 'y)))
      (is (= :a (look-up env3 'a)))
      (is (= < (look-up env3 '<))))))

(java-map)

;; (defprotocol P
;;   (h[_])
;;   (t[_])
;;   (h![_ v])
;;   (t![_ v])
;;   )
;;
;; (deftype Pair [^:volatile-mutable h ^:volatile-mutable t]
;;   P
;;   (h [_] h)
;;   (t[_] t)
;;   (h![p v] (set! h v))
;;   (t![p v] (set! t v))
;;   )

;; (defmethod eval-f 'cons [exp env] (Pair. (eval-f (second exp) env) (nth exp 2)))
;; (defmethod eval-f 'car [exp env] (h (eval-f (second exp) env)))
;; (defmethod eval-f 'cdr [exp env] (eval-f (t (eval-f (second exp) env)) env))

;; (testing "Pair"
;;     (is (= 1 (h (Pair. 1 2))))
;;     (is (= 2 (t (Pair. 1 2))))
;;     (is (= 3 (h! (Pair. 1 2) 3)))
;;     (let [p (Pair. 1 2)]
;;       (h! p 3)
;;       (is (= 3 (h p)))
;;       (is (= 2 (t p)))
;;       (t! p 4)
;;       (is (= 4 (t p)))
;;       )
;;     )
