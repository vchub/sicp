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
(defn pair? [x] (= (type x) (type (cns 1 2))))
(defn last-pair [p] (if (not (pair? (t p)))
                      p
                      (recur (t p))))
(defn eq? [x y] (if (and (pair? x) (pair? y))
                 (and (eq? (h x) (h y)) (eq? (t x) (t y)))
                 (= x y)))
(defn to-list [p] (cond
                    (nil? p) '()
                    (not (pair? p)) (list p)
                    :else (cons (h p) (to-list (t p)))))

(testing
  (is (pair? (cns '(1 2) nil)))
  (is (= '((1 2)) (to-list (cns '(1 2) nil))))
  (is (= '(1 2) (to-list (cns 1 2))))

 (let [l1 (cns 1 nil)
       l2 (cns 2 l1)]
   (is (= [1] (to-list l1)))
   (is (= [2 1] (to-list l2)))
   (is (eq? l1 (last-pair l1) ))
   (is (eq? l1 (last-pair l2) ))
   (is (= 2 (h l2)))
   (is (= l1 (t l2)))
   (is (= 1 (h (t l2))))
   (set-t! l1 3)
   (set-h! l2 '(1 2))
   (is (= '(1 2) (h l2)))))
