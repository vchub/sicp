(ns sicp.ch3.streams.stream-cons
  (:require [clojure.test :refer :all])
  (:import [java.io File]))

(defn mem-proc [proc]
  (let [run? (atom false)
        ret (atom nil)]
    (fn []
      (if @run?
        @ret
        (do (reset! ret (proc)) (reset! run? true) @ret)))))

(def h first)
(defn t [p] (((comp first rest) p)))

(deftest test-stream-basics
  (testing "code stream"
    (let [nums (fn nums [n]
                 [n (mem-proc (fn [] (nums (inc n))))])
          xs (nums 1)]
      (is (= 1 (h xs)))
      (is (= 2 ((comp h t) xs)))
      (is (= 3 ((comp h t t) xs))))

    (let [cnt (atom 0)
          x [1 (constantly nil)]
          y [2 (mem-proc (fn [] (swap! cnt inc) x))]]
      (is (= x (t y)))
      (is (= x (t y)))
      (is (= 1 @cnt))
      (is (= nil (t x))))

    (is (= 2 (t [1 (mem-proc (constantly 2))])))))

(test-stream-basics)

(defprotocol IStream
  (car [_])
  (cdr [_])
  (take-s [s n]))

(defrecord Stream [h t]
  IStream
  (car [_] h)
  (cdr [_] (t))
  (take-s [s n] (cond
                  (nil? s) nil
                  (< n 1) nil
                  :else
                  (cons (car s) (take-s (cdr s) (dec n)))))
  ;; clojure.lang.ISeq
  ;; (first [s] (car s))
  ;; (next [s] (cdr s))
  ;; (cons [s item] (Stream. item s))
  ;; clojure.lang.Seqable
  ;; (seq [this] this)
  )

(defmacro delay-m [form]
  `(mem-proc (fn [] ~form)))

(defmacro cons-m [h t]
  `(Stream. ~h (delay-m ~t)))

(defn cons-s [h t] (Stream. h (mem-proc t)))

(defn map-stream "fn, Stream -> Stream"
  [proc xs]
  (if (nil? xs)
    nil
    (cons-m
     (proc (car xs))
     (map-stream proc (cdr xs)))))

(defn map-streams "fn, [Stream] -> Stream"
  [proc & xss]
  (if (some nil? xss)
    nil
    (cons-m
     (apply proc (map car xss))
     (apply map-streams (cons proc (map cdr xss))))))

(deftest test-stream

  ;; (testing "clojure.lang.ISeq"
  ;;   (let [nums (fn nums [start step] (cons-m start (nums (+ start step) step)))
  ;;         xs (nums 1 1)
  ;;         ys (nums 2 2)]
  ;;     (is (= [2 4 6] (take-s ys 3)))
  ;;     (is (seq? xs))
  ;;     (is (= [2 4 6] (map identity ys)))
  ;;     (is (= [2 4 6] (take 3 (map identity ys))))))

  (testing "map-stream"
    (let [nums (fn nums [start step] (cons-m start (nums (+ start step) step)))
          xs (nums 1 1)
          ys (nums 2 2)]
      (is (= [2 4 6] (take-s (nums 2 2) 3)))
      (is (= [2 4 6] (take-s ys 3)))
      (is (= 2 (car ys)))
      (is (= Stream (type (cdr ys))))
      (is (= Stream (type xs)))
      (is (= [1 2 3] (take-s (map-stream identity xs) 3)))
      (is (= [3 6 9] (take-s (map-streams + xs ys) 3)))))

  (testing "cons-m"
    ;; (prn (macroexpand '(cons-m 1 (+ 1 2))))
    (let [x (cons-m 1 2)
          y (cons-m 3 x)
          lazy-nums (fn nums [start]
                      (cons-m start (nums (inc start))))
          xs (lazy-nums 3)]
      (is (= '(3 4 5 6) (take-s xs 4)))
      (is (= '(3 4 5 6) (take-s xs 4)))
      (is (= 1 (car x)))
      (is (= 2 (cdr x)))
      (is (= 3 (car y)))
      (is (= 2 (cdr (cdr y))))))

  (testing "code stream"
    (let [p (cons-s 1 (constantly 2))
          nums (fn nums [start] (cons-s start (fn [] (nums (inc start)))))
          xs (nums 2)]
      (is (= 1 (car p)))
      ;; (is (= 1 (next p)))
      (is (= 2 (cdr p)))
      (is (= 2 (car xs)))
      (is (= 3 (car (cdr xs))))
      (is (= '(2) (take-s xs 1)))
      (is (= '(2 3) (take-s xs 2)))
      (is (= '(2 3 4) (take-s xs 3)))))

  ;; (testing "misc"
  ;;   )

  ;; (is (= "foo" (with-out-str (prn "foo"))))
  ;; (prn (seq (.listFiles (File. "."))))
  ;; (prn (->> (File. ".")
  ;;           (.listFiles)
  ;;            ;; (seq)
  ;;           (map #(.getName %))))
  ;; (prn (->> (File. ".")
  ;;           (.list)
  ;;           (seq)))
  ;; (prn (count (file-seq (File. "."))))
  )

(test-stream)

(defmacro my-when [test & body]
  (list 'if test (cons 'do body)))

(deftest test-misc
  (let [fibs (->> (iterate (fn [[a b]] [b (+' a b)]) [0 1])
                  (map first))]
    (is (= [0 1 1 2 3 5 8] (take 7 fibs))))

  (testing "my-when"
    (is (my-when (< 0 2) false true))
    (is (= '(if (< 0 2) (do false true)) (macroexpand '(my-when (< 0 2) false true))))

    (is (= [5] (-> 25
                   (Math/sqrt)
                   (int)
                   (list))))))

(test-misc)

(comment
  (with-out-str (prn 'hi))

  (->)
  condp
  (reverse "some")
  (apply str (reverse "some"))

  (let [re (re-matcher #"\w+" "the red fox 22 some")]
    (re-find re))

  (re-seq #"\w+" "the red fox 22 some")

  (prn *ns*)
  (split-at 3 (range 10))
  (split-with even? (range 10))
  (prn 'x)
  (prn `x)
  (clojure.repl/apropos "mem")
  (clojure.repl/find-doc "mem")
  (clojure.repl/doc cons))
