(ns sicp.ch2.trees
  (:require [clojure.test :refer :all]))

(defn leaf? [x] (number? x))

(defn deep-reverse [xs]
  (loop [xs xs acc nil]
    (cond
    (empty? xs) acc
    (leaf? (first xs)) (recur (rest xs) (cons (first xs) acc))
    :else (recur (rest xs) (cons (deep-reverse (first xs)) acc))
    )))

(testing
  (is (= '((4 3) 2 1) (deep-reverse '(1 2 (3 4)))))
  (is (= `((4 3) 2 1 (0)) (deep-reverse `((0) 1 2 (3 4)))))
  (is (seq? (list 1)))
  ;; (is (seq? [1]))
  (is (= [1 2 3] (concat `(1 2) `(3))))
  (is (= '((3) 1 2) (conj `(1 2) `(3))))
  (is (= (list `(3) 1 2) (conj `(1 2) `(3))))
  (is (= [1 2 [3]] (conj [ 1 2 ] [ 3 ])))
  (is (= [1 2] (conj [1] 2)))
  )

(defn fringe
  [xs]
  (loop [xs xs acc []]
    (cond
      (empty? xs) acc
    (leaf? (first xs)) (recur (rest xs) (conj acc (first xs)))
    :else (recur (concat (first xs) (rest xs)) acc)
    )))

(testing
  (is (= [1 2 3] (fringe [1 2 3])))
  (is (= [1 2 3] (fringe [1 '(2 3)])))
  (is (= [1 2 3 4] (fringe [1 '(2 3) [4]])))
  )
