(ns sicp.ch3.streams.macros
  (:require [clojure.test :refer :all]))

(defn unless-fn [test-e exp-fn]
  (if test-e nil (exp-fn)))

(defn unless-fn-1 [test-e & exps]
  (if test-e nil (last exps)))

(defmacro unless-m [test & exps]
  ;; (list 'if (list 'not test) (cons 'do exps))
  (list 'if test nil (cons 'do exps)))

(defmacro unless-m-1 [test body]
  ;; (list 'if (list 'not test) (cons 'do body))
  (list 'if test nil body))

(defmacro chain [obj method & methods]
  (loop [methods methods acc (list '. obj method)]
    (if (empty? methods)
      acc
      (recur (rest methods) (list '. acc (first methods))))))

(defmacro chain [obj method & methods]
  (loop [methods methods acc `(. ~obj ~method)]
    (if (empty? methods)
      acc
      (recur (rest methods) `(. ~acc ~(first methods))))))

;; (defmacro chain
;;   ([x form] (list '. x form))
;;   ([x form & more] (concat (list 'chain (list '. x form)) more))
;;   ;; ([x form & more] (concat (list 'chain x form) more))
;;   )


(defmacro chain
  ([x form] `(. ~x ~form))
  ([x form & more] `(chain (. ~x ~form) ~@more)))

(defmacro bench [exp]
  `(let [start# (System/nanoTime)
         result# ~exp]
         {:result result# :elapsed (- (System/nanoTime) start#)}))

(deftest test-unless

  (testing "bench"
    ;; (prn (macroexpand-1 '(bench (str "a" "b"))))
    (prn (macroexpand '(bench (str "a" "b"))))
    ;; (is (= `(let [start (System/nanoTime)
    ;;               result (str "a" "b")]
    ;;           {:result result :elapsed (- (System/nanoTime) start)})
    ;;        (macroexpand '(bench (str "a" "b")))))
    (is (= "ab" (:result (bench (str "a" "b")))))
    (is (> 1e4 (:elapsed (bench (str "a" "b")))))
    )

  (testing "chain"
    (is (= '(. arm getHand) (macroexpand '(chain arm getHand))))
    (is (= '(. (. (. arm getHand) getFinger) getNail)
           (macroexpand '(chain arm getHand getFinger getNail)))))

  (testing "~@"
    (let [xs [1 2]]
      (is (= `(x 1 2) `(x ~@xs)))
      (is (= `(x [1 2]) `(x ~xs)))
      ;; (is (= '(x [1 2]) '(x ~xs)))
      ))

  (testing "unless-m-1"
  ;; (prn (macroexpand-1 '(unless-m (< 0 1) 3 2 1)))
  ;; (prn (macroexpand '(unless-m (< 0 1) 3 2 1)))
    (is (= nil (unless-m-1 (< 0 1) (do 3 2 1))))
    (is (= 1 (unless-m-1 (> 0 1) (do 3 2 1)))))

  (testing "unless-m"
  ;; (prn (macroexpand '(unless-m (< 0 1) 3 2 1)))
    (is (= nil (unless-m (< 0 1) 3 2 1)))
    (is (= 1 (unless-m (> 0 1) 3 2 1))))

  (testing "unless-fn-1"
    (is (= nil (unless-fn-1 (< 0 1) 3 2 1)))
    (is (= 1 (unless-fn-1 (> 0 1) 3 2 1))))
  (testing "unless-fn"
    (is (= nil (unless-fn (< 0 1) (fn [] 2 3 1))))
    (is (= 1 (unless-fn (> 0 1) (fn [] 2 3 1))))))

(test-unless)

(comment
  (macroexpand '(.. arm getHand getFinger getNail))
  (macroexpand '(and 1 2 3))
  (time (str "a" "b")))
