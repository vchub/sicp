(ns sicp.ch2.poly.poly2
  (:require [clojure.test :refer :all]
            [clojure.math.combinatorics :as combo]))

;; dispatch table
(def f-tbl (atom {}))

(defn put [op a-types f]
  (swap! f-tbl assoc-in [op a-types] f))

(defn atype [x]
  (let [t (or
           (:t (meta x))
           (:t x))]
    (cond
      (not (nil? t)) t
      (= (type x) Long) 'Long
      (number? x) 'real
      (symbol? x) 'symbol
      :else nil)))

;; defined later
(def raise)
(def project)

(defn log [x] (do (prn x) x))

(defn ancesstors
  "elem -> [symbol]"
  [x]
  (if (nil? x)
    nil
    (cons x (ancesstors (raise x)))))

(defn ancesstor-op [op args]
  (loop [args-s (->> (map ancesstors args)
                     (apply combo/cartesian-product))]
    (if (empty? args-s)
      (throw (Exception. (str "no method for these op args: " op " " (seq args-s))))
      (let [a-types (map atype (first args-s))
            proc (get-in @f-tbl [op a-types])]
        (if (not (nil? proc))
          (apply proc (first args-s))
          (recur (rest args-s)))))))

(defn apply-genric [op & args]
  (let [a-types (map atype args)
        proc (get-in @f-tbl [op a-types])]
    (if proc
      (apply proc args)
      (ancesstor-op op args))))

(defn gen-with-drop [args]
  (loop [res (apply apply-genric args) prj (project res)]
    (cond
      (not= res (raise prj)) res
      (= res prj) res
      :else (recur prj (project prj)))))

(defn mul [x y] (gen-with-drop ['mul x y]))
(defn div [x y] (gen-with-drop ['div x y]))
(defn add [x y] (gen-with-drop ['add x y]))
(defn sub [x y] (gen-with-drop ['sub x y]))

(defn raise [x] (apply-genric 'raise x))
(defn project [x] (apply-genric 'project x))

(defn install-real []
  (letfn [(add [x y] (+ x y))
          (sub [x y] (- x y))
          (mul [x y] (* x y))
          (div [x y] (/ x y))
          (project [x] (Math/round x))]
    (put 'raise '(real) (fn [x] nil))
    (put 'project '(real) project)
    (put 'add '(real real) add)
    (put 'sub '(real real) sub)
    (put 'mul '(real real) mul)
    (put 'div '(real real) div)))

(install-real)

(defn install-Long []
  (put 'raise '(Long) (fn [x] (double x)))
  (put 'project '(Long) identity)
  'done)

(install-Long)

(testing
 (is (= 8 (mul 4.0 2)))
  (is (= 8.2 (mul 4.1 2)))
  (is (= 2 (div 4 2.0)))
  (is (= 6 (add 4 2.0)))
  (is (= -1 (sub 1 2.0))))

(comment
  (defrecord T [a])
  (type (->T 1))
  (log 3)
  (->> (map ancesstors [1 2]))

  (= (type 1) Long)
  (meta '())
  (meta #'+)
  (meta #'/)
  (flatten [[1] [2]])
  (flatten [[[1]] [2]])
  (mapcat identity [[[1]] [2]]))
