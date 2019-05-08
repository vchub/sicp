(ns sicp.ch1.ch_1_3
  (:require [clojure.test :refer :all]))

(defn sum-rec
  "(num->num) num (num->num) num"
  [term a nxt b]
  (if (> a b)
    0
    (+ (term a) (sum-rec term (nxt a) nxt b))))

(testing
 (let [add2 (fn [a] (+ a 2))]
   (is (= 1 (sum-rec identity 0 inc 1)))
   (is (= 6 (sum-rec identity 0 inc 3)))
   (is (= 55 (sum-rec identity 0 inc 10)))
   (is (= (reduce + (range 11)) (sum-rec identity 0 inc 10)))
   (is (= 6 (sum-rec identity 0 add2 5)))
   (is (= 14 (sum-rec add2 0 inc 3)))
   (is (= 3025 (sum-rec #(* % % %) 0 inc 10)))
   ))

(defn integral
  [f a b dx]
  (let [nxt-x (fn[x](+ x dx))]
    (* (sum-rec f (+ a (/ dx 2)) nxt-x b) dx)))

(defn sqr [x] (* x x))
(defn cube [x] (* x x x))

(testing
  (let [t [{:got (integral sqr 0 1 0.1) :exp 0.333}
           {:got (integral cube 0 1 0.1) :exp 0.25} ]
        eps 0.01]
    (doseq [{:keys [got exp]} t
            ]
      ;; (prn got exp)
      (is (and (< got (+ exp eps)) (> got (- exp eps))))))

  )

(comment
  (integral cube 0 1 0.1))
