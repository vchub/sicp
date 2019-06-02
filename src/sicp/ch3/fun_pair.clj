(ns sicp.ch3.fun-pair
  (:require [clojure.test :refer :all]))

;; Pair [h t] or [car cdr]

(defn cns [h t]
  (let [h (atom h)
        t (atom t)]
    (fn dispatch [msg]
      (cond
        (= 'set-h! msg) (fn [x] (reset! h x))
        (= 'set-t! msg) (fn [x] (reset! t x))
        (= 'h msg) @h
        (= 't msg) @t
        :else (throw (Exception. (str "Undefined operation on CNS " msg)))))))

(defn h [pair] (pair 'h))
(defn set-h! [pair x] ((pair 'set-h!) x))
(defn t [pair] (pair 't))
(defn set-t! [pair x] ((pair 'set-t!) x))

(testing
 (let [l1 (cns 1 nil)
       l2 (cns 2 l1)]
   (is (= 2 (h l2)))
   (is (= l1 (t l2)))
   (is (= 1 (h (t l2))))
   (set-t! l1 3)
   (set-h! l2 '(1 2))
   (is (= '(1 2) (h l2)))))
