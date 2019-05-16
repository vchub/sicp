(ns sicp.ch2.church_num
 (:require [clojure.test :refer :all]) )


(def zero (fn [f] (fn[x] x)))
(defn iszero? [n] (= ((n n) 0) 0))
(defn add-1 [n] (fn[f] (fn[x] (f ((n f) x)))))

(def one (fn [f] (fn[x] (f x))))
(def two (fn [f] (fn[x] (f (f x)))))

(defn add [a b] (fn [f] (fn [x] ((a f) ((b f) x)))))

(defn church->int [cn] ((cn inc) 0))

(testing
  (let [three (add-1 two)]
    (is (iszero? zero))
    (is (not (iszero? one)))
    (is (= 0 (church->int zero)))
    (is (= 1 (church->int one)))
    (is (= 3 (church->int three)))
    (is (= 4 (church->int (add three one))))
    ))


;; (defn inc-n [n] (fn [] n))
;; (defn dec-n [n] (if (iszero? n) zero (n)))
;;
;; (defn display [n] (loop [n n i 0]
;;                     (if (iszero? n)
;;                       i
;;                       (recur (dec-n n) (inc i)))))
;;
;; (defn equal [n m] (= (display n) (display m)))
;;
;; (defn add [n m] (loop [n n m m]
;;                     (cond
;;                       (iszero? m) n
;;                       :else (recur (inc-n n) (dec-n m)))))

;; (testing
;;   (let [one (inc-n zero)
;;         two (inc-n (inc-n zero))
;;         three (inc-n two)
;;         ]
;;     (is (iszero? zero))
;;     (is (not (iszero? two)))
;;     (is (= 0 (display zero)))
;;     (is (= 1 (display one)))
;;     (is (= 2 (display two)))
;;     (is (= 3 (display three)))
;;     (is (not (equal two three)))
;;     (is (equal two (dec-n three)))
;;     (is (= 5 (display (add three two))))
;;     ))

(comment
  (let [p (fn[] true)]
    (if (= true p) 1 2)))
