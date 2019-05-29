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
(def zero-e?)
(def equal?)
(def poly)

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
      (not (equal? res (raise prj))) res
      (equal? res prj) prj
      :else (recur prj (project prj)))))

(defn mul [x y] (gen-with-drop ['mul x y]))
(defn div [x y] (gen-with-drop ['div x y]))
(defn add [x y] (gen-with-drop ['add x y]))
(defn sub [x y] (gen-with-drop ['sub x y]))

(defn raise [x] (apply-genric 'raise x))
(defn project [x] (apply-genric 'project x))
(defn zero-e? [x] (apply-genric 'zero-e? x))
(defn equal? [x y] (apply-genric 'equal? x y))

(defn install-real []
  (letfn [(add [x y] (+ x y))
          (sub [x y] (- x y))
          (mul [x y] (* x y))
          (div [x y] (/ x y))
          ;; (project [x] (Math/round x))
          ]
    ;; (put 'raise '(real) (fn [x] nil))
    (put 'raise '(real) (fn [x] (poly 'x [[0 x]])))
    (put 'project '(real) long)
    (put 'zero-e? '(real) zero?)
    (put 'equal? '(real real) =)
    (put 'add '(real real) add)
    (put 'sub '(real real) sub)
    (put 'mul '(real real) mul)
    (put 'div '(real real) div)))

(install-real)

(defn install-Long []
  (put 'raise '(Long) double)
  (put 'project '(Long) long)
  'done)

(install-Long)

(defrecord Poly [t v terms])

(defn install-poly []
  (letfn [(p [t] (first t))
          (c [t] (second t))
          (zero-t? [t] (zero-e? (c t)))
          (zero-p? [x] (empty? (filter (complement zero-t?) (:terms x))))
          (make [v terms] (Poly. 'poly v (->> (map vec terms)
                                              (filter (complement zero-t?))
                                              (sort))))
          (project [x] (c (first (filter #(zero? (p %)) (:terms x)))))

          (equal-p? [x y]
                    (loop [xs (:terms x) ys (:terms y)]
                      (cond
                        (and (empty? xs) (empty? ys)) true
                        (or (empty? xs) (empty? ys)) false
                        (and (equal? (p (first xs)) (p (first ys)))) (recur (rest xs) (rest ys))
                        :else false)))

          (same-var? [x y] (= (:v x) (:v y)))
          (add-t [x y] (if (= (p x) (p y))
                         [(p x) (add (c x) (c y))]
                         (throw (Exception. (str "Can't add terms " x y)))))
          (add-terms [xs ys]
                     (cond
                       (empty? xs) ys
                       (empty? ys) xs
                       (< (p (first xs)) (p (first ys))) (cons (first xs) (add-terms (rest xs) ys))
                       (> (p (first xs)) (p (first ys))) (cons (first ys) (add-terms xs (rest ys)))
                       :else (cons (add-t (first xs) (first ys)) (add-terms (rest xs) (rest ys)))))

          (add-p [x y] (if (same-var? x y)
                         (make (:v x) (add-terms (:terms x) (:terms y)))
                         (throw (Exception. (str "Can't add Poly with different vars " (:v x) " " (:v y))))))

          (mul-t [t ts] (map #(list (add (p %) (p t)) (mul (c %) (c t))) ts))

          (mul-p [x y] (if (same-var?  x y)
                         (make (:v x)
                               (reduce add-terms
                                       (map #(mul-t % (:terms y)) (:terms x))))
                         (throw (Exception. (str "Can't add terms " x y)))))]

    (put 'raise '(poly) (fn [x] nil))
    (put 'equal? '(poly poly) equal-p?)
    (put 'make 'poly make)
    (put 'zero-e? '(poly) zero-p?)
    (put 'project '(poly) project)
    (put 'add '(poly poly) add-p)
    (put 'mul '(poly poly) mul-p)
    ;; (put 'mul-t '(term poly) mul-t)
    'done))

(install-poly)

(defn poly [v terms] ((get-in @f-tbl '(make poly)) v terms))

(testing
 (is (zero-e? (poly 'x '((1 0)))))
  (is (= '((0 1) (2 3)) (:terms (poly 'x '((1 0) (2 3) (0 1))))))
  ;; project
  (is (= 2 (project (poly 'x '((1 0) (0 2))))))
  (is (= 2 (project (poly 'x [[1 0] [0 2]]))))
  ;; equal?
  (is (equal? (poly 'x [[1 0] [0 2]]) (poly 'x [[1 0] [0 2]])))
  (is (equal? (poly 'x [[1 0] [0 2]]) (poly 'x '([1 0] [0 2]))))
  (is (equal? (poly 'x [[1 0] [0 2]]) (poly 'x '([0 2] [1 0]))))
  (is (equal? 2 (poly 'x '([0 2] [1 0]))))
  (is (equal? (raise (raise 2)) (poly 'x '([0 2] [1 0]))))
  (is (not (equal? (poly 'x [[1 1] [0 2]]) (poly 'x '([1 0] [0 2])))))
  ;; add
  (is (= 2 (add (poly 'x '((0 1) (1 2))) (poly 'x '((0 1) (1 -2))))))
  (is (= (poly 'x '((0 2) (1 2))) (add 1 (poly 'x '((0 1) (1 2))))))
  ;; TODO: fix negative
  ;; (is (= (poly 'x '((1 2))) (add -1 (poly 'x '((0 1) (1 2))) )))
  ;; mul
  (is (= (poly 'x '((0 1) (2 -4))) (mul (poly 'x '((0 1) (1 2))) (poly 'x '((0 1) (1 -2))))))
  (is (= (poly 'x '((0 2) (1 5) (2 2))) (mul (poly 'x '((0 1) (1 2))) (poly 'x '((0 2) (1 1))))))
  (is (= (poly 'x '((0 4) (1 2))) (mul 2 (poly 'x '((0 2) (1 1)))))))

(testing
 (is (= 8 (mul 4.0 2)))
  (is (= 8.2 (mul 4.1 2)))
  (is (= 2 (div 4 2.0)))
  (is (= 6 (add 4 2.0)))
  (is (= -1 (sub 1 2.0)))
  (is (zero-e? (sub 1 1.0))))

(comment
  (sort '(1 3 2))
  (sort '((1 2) (0 3)))
  (sort [1 3 2])
  (sort [[2 0] [1 2] [3 1] [1 1]])
  (reverse (sort [[2 0] [1 2] [3 1] [1 1]]))

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
