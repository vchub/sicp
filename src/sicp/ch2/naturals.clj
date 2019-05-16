(ns sicp.ch2.naturals
 (:require [clojure.test :refer :all]) )


(def zero (fn [] true))
(defn iszero? [n] (= true (n)))
(defn inc-n [n] (fn [] n))
(defn dec-n [n] (if (iszero? n) zero (n)))

(defn display [n] (loop [n n i 0]
                    (if (iszero? n)
                      i
                      (recur (dec-n n) (inc i)))))

(defn equal [n m] (= (display n) (display m)))

(defn add [n m] (loop [n n m m]
                    (cond
                      (iszero? m) n
                      :else (recur (inc-n n) (dec-n m)))))

(testing
  (let [one (inc-n zero)
        two (inc-n (inc-n zero))
        three (inc-n two)
        ]
    (is (iszero? zero))
    (is (not (iszero? two)))
    (is (= 0 (display zero)))
    (is (= 1 (display one)))
    (is (= 2 (display two)))
    (is (= 3 (display three)))
    (is (not (equal two three)))
    (is (equal two (dec-n three)))
    (is (= 5 (display (add three two))))
    ))

(comment
  (let [p (fn[] true)]
    (if (= true p) 1 2)))
