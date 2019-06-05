(ns sicp.ch3.memoize
  (:require [clojure.test :refer :all]))

(defn my-mem [f]
  (let [cache (atom {})]
    (fn [& args]
      (let [r (get @cache args)]
        (or r
            (let [r (apply f args)]
              ;; (prn "called, args" args "r" r)
              (swap! cache assoc args r)
              r))))))

(def m-fib (my-mem (fn [n] (condp = n
                             0 1
                             1 1
                             (+ (m-fib (- n 1)) (m-fib (- n 2)))))))

(def fib (memoize (fn [n] (condp = n
                             0 1
                             1 1
                             (+ (fib (- n 1)) (fib (- n 2)))))))
(testing
 (is (= 1 (m-fib 0)))
  (is (= 1 (m-fib 0)))
  (is (= 5 (m-fib 4)))
  (is (= 8 (fib 5)))
  )
