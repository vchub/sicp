(ns sicp.ch3.fun-pair
  (:require [clojure.test :refer :all]))

;; Pair [h t] or [car cdr]

(defn cns [h t]
  (fn dispatch [message]
    (cond
      (= 'h message) h
      (= 't message) t
      :else (throw (Exception. (str "Undefined operation on CNS " message))))))

(defn h [pair] (pair 'h))
(defn t [pair] (pair 't))

(testing
 (let [l1 (cns 1 nil)
       l2 (cns 2 l1)]
   (is (= 2 (h l2)))
   (is (= l1 (t l2)))
   (is (= 1 (h (t l2))))))
