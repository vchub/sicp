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
   (is (= 3025 (sum-rec #(* % % %) 0 inc 10)))))

(defn integral
  [f a b dx]
  (let [nxt-x (fn [x] (+ x dx))]
    (* (sum-rec f (+ a (/ dx 2)) nxt-x b) dx)))

(defn sqr [x] (* x x))
(defn cube [x] (* x x x))

(testing
 (let [t [{:got (integral sqr 0 1 0.1) :exp 0.333}
          {:got (integral cube 0 1 0.1) :exp 0.25}]
       eps 0.01]
   (doseq [{:keys [got exp]} t]
      ;; (prn got exp)
     (is (and (< got (+ exp eps)) (> got (- exp eps)))))))

(defn lazy-4-2
  [n]
  (if (zero? n)
    nil
    (lazy-seq (cons (if (even? n) 4 2) (lazy-4-2 (dec n))))))

(defn simpson-seq
  [f a n h]
  (letfn [(gen [n x]
            (if (zero? n)
              nil
              (lazy-seq (cons (if (even? n)
                                (* 4 (f x))
                                (* 2 (f x))) (gen (dec n) (+ a h))))))]
    (gen n a)))

(defn simpson-integral
  [f a b n]
  (reduce + (simpson-seq f a n (/ (+ a b) n))))

(defn simpson
  [f a b n]
  (let [h (/ (- b a) n)
        term (fn [k]
               (cond
                 (zero? k) (f a)
                 (= n k) (f b)
                 (even? k) (* 2 (f (+ a (* k h))))
                 :else (* 4 (f (+ a (* k h))))))]
    (* (/ h 3) (reduce + (map term (range (inc n)))))))

(testing
 (is (= [4 2] (take 2 (lazy-4-2 2))))
  (is (= [4 2 4 2] (lazy-4-2 4)))
  (is (= [0 0.2] (simpson-seq identity 0 2  0.1)))
  (is (= 0.2 (simpson-integral identity 0 0.2 2)))
  (is (= 1/4 (simpson cube 0 1 100)))
  (is (= 1/3 (simpson sqr 0 1 100))))

(defn sum-iter
  [term a nxt b]
  (letfn [(iter [a result]
            (if (> a b)
              result
              (iter (nxt a) (+ result (term a)))))]
    (iter a 0)))

(testing
 (is (= 55 (sum-iter identity 0 inc 10)))
 )



(comment
  (integral cube 0 1 0.1)
  (map-indexed vector (range 0 1 0.2)))
