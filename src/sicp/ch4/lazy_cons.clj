(ns sicp.ch4.lazy-cons
  (:require [clojure.test :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [sicp.ch4.eval-data-multi :refer :all
             :exclude [make-env apply-f eval-seq]]))

(def force-f)
(def apply-f)

(defn make-env [] (let [env @(sicp.ch4.eval-data-multi/make-env)
                        env (assoc env 'force force-f)]
                    (atom env)))

(defrecord P [h t])

(defn P-> [h t] (atom (P. h t)))

(defn set-env-var! [env v-name v-val] (swap! env assoc v-name v-val))

(defn memoize-f "memoize no args procedure"
  [proc]
  (let [ret (atom nil)]
    (fn [] (if @ret @ret (do
                           (reset! ret (proc))
                         ;; (prn @ret)
                           @ret)))))

(defmethod eval-f 'fn [exp env] exp)

(defn eval-seq [exp-seq env]
  (reduce (fn [acc exp] (eval-f exp env)) nil exp-seq))

(defmethod eval-f 'do [exp env] (eval-seq (next exp) env))

(defmethod eval-f 'def [exp env]
  (let [k (second exp)
        v (eval-f (nth exp 2) env)]
    (set-env-var! env k v)))

(defmethod eval-f 'set! [exp env]
  (let [k (second exp)]
    (if (contains? @env k)
      (set-env-var! env k (eval-f (nth exp 2) env))
      (throw (Exception. (str "Can't set undefined var " k))))))

(defmethod eval-f 'cons [exp env] (P-> (eval-f (second exp) env) (nth exp 2)))
(defmethod eval-f 'car [exp env] (:h @(eval-f (second exp) env)))

(defmethod eval-f 'car! [exp env]
  (let [a (eval-f (second exp) env)]
    (swap! a assoc :h (eval-f (nth exp 2) env))))

(defmethod eval-f 'cdr [exp env] (eval-f (:t @(eval-f (second exp) env)) env))
(defmethod eval-f 'cdr! [exp env]
  (let [a (eval-f (second exp) env)]
    (swap! a assoc :t (nth exp 2))))

(defn delayed? [exp] (and (seq? exp)
                          (= 'delayed (first exp))))

(defn force-f [exp]  (if (delayed? exp)
                       (if (delayed? (second exp))
                         (recur (second exp))
                         ((second exp)))
                       exp))

(defn delay-> [exp env]  (list 'delayed (memoize-f (fn [] (eval-f exp env)))))

(defmethod eval-f 'delay [exp env] (delay-> (second exp) env))
(defmethod eval-f 'delayed [exp env] exp)

(defmethod eval-f :default [exp env] (apply-f
                                       ;; fn
                                      (eval-f (first exp) env)
                                       ;; args
                                      ;; (next exp)
                                      (map #(eval-f % env) (next exp))
                                      env))

(defmulti apply-f (fn [proc args env]
                    (cond
                      (seq? proc) 'compound-proc
                      (and (seq? proc) (= 'force (first proc))) 'primitive-proc
                      :else 'primitive-proc)))

(defmethod apply-f 'compound-proc [proc args env]
  (let [body (proc-body proc)
        ;; args (map (fn [x] (if (delayed? x) x (delay-> x env))) args)
        env (extend-env env (proc-params proc) args)
        ;; ret (delay-> (eval-seq body env) env)]
        ret (delay-> (list* 'do body) env)]
        ;; ret (eval-f (list* 'do body) env)]
        ;; ret (eval-seq body env)]
    ret))

(defmethod apply-f 'primitive-proc [proc args env]
  ;; (prn "---" proc args)
  (let [args (map force-f args)]
    ;; (prn "in primitive-proc" proc args)
    (apply proc args)))

(deftest test-lazy-cons

  (testing "proc args lazy eval"
    (let [env (make-env)
          eval-f (fn [exp] (eval-f exp env))]
      (is (= 2 (eval-f '(+ 1 1))))
      (is (= 2 (eval-f '(force 2))))
      (is (= 2 (eval-f '(force (+ 1 1)))))
      (is (= 'delayed (first (eval-f '(delay (+ 1 1))))))
      (is (= 2 ((second (eval-f '(delay (+ 1 1)))))))
      (is (= 2 (eval-f '(force (delay (+ 1 1))))))
      (is (= 9 (eval-f '(do
                          (def x-x 4)
                          (def x-d (delay (+ x-x 5)))
                          (force x-d)))))

      (is (= 2 (eval-f '(+ 1 (delay 1)))))
      (is (= 2 (eval-f '(do
                          (def x (delay 1))
                          (+ x 1)))))

      (is (= 2 (eval-f '(force (delay (delay (+ 1 1)))))))

      (eval-f '(def ident (fn [x]  x)))
      (is (delayed? (eval-f '(ident 1))))
      ;; (is (= 10 (eval-f '(force (ident 10)))))
      (is (= 3 (eval-f '(+ (ident 2) (ident 1)))))
      (is (= 3 (eval-f '(+ (ident 2) (ident 1)))))
      ;; (eval-f '(def f-ident (fn [x] (prn "in f-ident" x) x)))
      ;; (is (delayed? (eval-f '(f-ident (ident 3)))))
      ;;
      (eval-f '(def sum (fn [x y] (+ x y))))
      (is (delayed? (eval-f '(sum (ident 20) (ident 11)))))
      (is (= 31 (eval-f '(+ 0 (sum (ident 20) (ident 11))))))

      (is (= 31 (eval-f '(do (+ 0 (sum (ident 20) (ident 11)))))))

      ;; (is (= 2 (eval-f '(force (force (fn[]
      ;;                     (def inc- (fn [x] (+ 1 x)))
      ;;                     (def i-f (fn [x]  x))
      ;;                     (+ 0 (i-f (inc- 1)))))
      ;;                     ))))

      ;; TODO: fix-it
      ;; (is (= 2 (eval-f '(+ 1 (delay (delay 1))))))

      (is (= 2 (eval-f '(do
                          (def inc- (fn [x] (+ 1 x)))
                          (inc- 1)
                          (+ 0 (inc- 1))
                          (def f (fn [x] (inc- x)))
                          (+ 0 (force (force (f 1))))
                          (def i-f (fn [x] (prn x) x))
                          (+ 0 (force (i-f (inc- 1))))
                          ))))

      ;; (is (= 2 (eval-f '(do
      ;;                     (def ax 1)
      ;;                     (+ ax ax)))))
      ;; (eval-f '(def inc (fn[x] (prn x) (+ 1 x))))
      ;; (eval-f '(def y 1))
      ;; (is (= 2 (eval-f '(inc y))))
      ))

  (testing "memoize-f"
    (let [x 1 y 2
          f (memoize-f (fn [] (+ x y)))]
      (is (= 3 (f)))))

  ;; (testing "list"
  ;;   (let [env (make-env)
  ;;         eval-f (fn [exp] (eval-f exp env))]
  ;;     (eval-f '(do
  ;;                (def range (fn [start end step]
  ;;                             (def iter (fn [acc x]
  ;;                                         (if (< x start)
  ;;                                           acc
  ;;                                           (iter (cons x acc) (- x step)))))
  ;;                             (iter nil end)))
  ;;                (def r1 (range 1 10 1))))
  ;;     (is (= 1 (eval-f '(car r1))))
  ;;     ;; TODO: fix lazy eval
  ;;     ;; (is (= 2 (eval-f '(car (cdr r1)))))
  ;;     ))


  (testing "cons and car cdr car!"
    (let [env (make-env)
          eval-f (fn [exp] (eval-f exp env))]
      (eval-f '(def y 2))
      (is (= 2 (eval-f 'y)))
      (eval-f '(def p1 (cons 1 x)))
      (is (= 1 (eval-f '(car p1))))
      (is (thrown? Exception (eval-f '(cdr p1))))
      (eval-f '(def x 2))
      (is (= 2 (eval-f '(cdr p1))))

      (eval-f '(def p2 p1))
      (is (= 1 (eval-f '(car p2))))
      (eval-f '(car! p1 3))
      (is (= 3 (eval-f '(car p1))))
      (is (= 3 (eval-f '(car p2))))

      (is (= 2 (eval-f '(cdr p2))))
      (eval-f '(cdr! p2 xx))
      (is (thrown? Exception (eval-f '(cdr p1))))
      (eval-f '(def xx 0))
      (is (= 0 (eval-f '(cdr p2))))
      (is (= 0 (eval-f '(cdr p1))))

      (eval-f '(def p3 (cons 4 p1)))
      (is (= 4 (eval-f '(car p3))))
      (is (= 3 (eval-f '(car (cdr p3)))))
      (is (= 0 (eval-f '(cdr (cdr p3)))))))

  (testing "def set!"
    (let [env (make-env)
          eval-f (fn [exp] (eval-f exp env))]
      (eval-f '(def y 2))
      (is (= 2 (eval-f 'y)))
      (is (thrown? Exception (eval-f '(set! x 0))))
      (eval-f '(set! y 4))
      (is (= 4 (eval-f 'y))))))

(test-lazy-cons)


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
