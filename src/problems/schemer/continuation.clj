(ns problems.schemer.continuation
  (:require [clojure.test :refer :all]))

(deftest multirember
  (testing "loop"
    (defn multirember [x xs]
      (loop [xs xs acc []]
        (cond
          (empty? xs) acc
          (= x (first xs)) (recur (rest xs) acc)
          :else (recur (rest xs) (conj acc (first xs))))))

    (is (= [1 2] (multirember 3 [1 3 3 2 3]))))

  (testing "recursion"
    (defn multirember [x xs]
      (cond
        (empty? xs) xs
        (= x (first xs)) (multirember x  (rest xs))
        :else (cons (first xs) (multirember x (rest xs)))))

    (is (= [1 2] (multirember 3 [1 3 3 2 3]))))

  (testing "continuation"
    (defn multirember [x xs]
      (((fn [rm-fn]
          ((fn [f] (f f))
           (fn [f] (rm-fn (fn [x] ((f f) x))))))
        (fn [rm]
          (fn [xs]
            (cond
              (empty? xs) xs
              (= x (first xs)) (rm (rest xs))
              :else (cons (first xs) (rm (rest xs))))))) xs))

    (is (= [1 2 5] (multirember 3 [1 3 3 2 3 5])))))

(multirember)

(comment
  (if true (/ 1 1) (/ 2 0))
  (or true (/ 2 0))
  (and false (/ 2 0)))
