(ns sicp.ch2.poly.poly
  (:require [clojure.test :refer :all]
            [sicp.ch2.poly.arith :as arith :refer [put apply-genric]]
            [clojure.set :as s]))

;; term [power, coeff]
;; (defrecord Term [p c])
;;
;; (defn comp-term [x y] (compare (:p x) (:p y)))
;;
;; (defn add-term [x y] (if (zero? (comp-term x y))
;;                        (Term. (:p x) (+ (:c x) (:c y)))
;;                        (throw (Exception. (str "Can't add terms " x y)))))

;; terms are ordered by power
(defrecord Poly [t v terms])

(defn install-poly []
  (letfn [(make [v terms] (Poly. 'poly v terms))
          ;; (make [v terms] ^{:t 'poly} (Poly. v terms))
          (same-v [x y] (= (:v x) (:v y)))
          (p [t] (first t))
          (c [t] (second t))
          (add-t [x y] (if (= (p x) (p y))
                         [(p x) (+ (c x) (c y))]
                         (throw (Exception. (str "Can't add terms " x y))))
            )
          (add-terms [xs ys]
                     (cond
                       (empty? xs) ys
                       (empty? ys) xs
                       (< (p (first xs)) (p (first ys))) (cons (first xs) (add-terms (rest xs) ys))
                       (> (p (first xs)) (p (first ys))) (cons (first ys) (add-terms xs (rest ys)))
                       :else (cons (add-t (first xs) (first ys)) (add-terms (rest xs) (rest ys)))))
          (add [x y] (if (same-v x y)
                       (make (:v x) (add-terms (:terms x) (:terms y)))
                       (throw (Exception. (str "Can't add Poly with different vars " (:v x) " " (:v y))))))
          ;; (mul-t [p t])
          ]

    (put 'make 'poly make)
    (put 'add '(poly poly) add)
    'done))

(install-poly)

(defn poly [v terms] ((get-in @arith/f-tbl '(make poly)) v terms))
;; (defn add [x y] (apply-genric 'add x y))

(testing
 (let [p (poly 'x '((0 2) (2 1)))
       p2 (poly 'x '((0 2) (1 1) (2 1)))]
   (is (= 'poly (arith/atype p)))
   (is (= '((0 2) (2 1)) (:terms p)))
   (is (thrown? Exception  (arith/add p (poly 'y '((0 2))))))
   (is (= '((0 3)) (:terms (arith/add (poly 'x '((0 2))) (poly 'x '((0 1)))))))
   (is (= '((0 4) (1 1) (2 2)) (:terms (arith/add  p p2))))
   ;; (is (= 1 p))
   ))

;; (swap! f-tbl assoc :b 1)
;; (testing
;;   (is (= 0 (:t @f-tbl)))
;;   (is (= 1 (:b @f-tbl)))
;;   (is (= 1 (:b @arith/f-tbl)))
;;   )

(comment
  (letfn [(f [xs] (if (empty? xs) xs (cons (first xs) (f (rest xs)))))]
    (f [3 2]))

  (meta (let [t ^:hi (Term. 1 2)]
          t))
  (meta ^{:t 'hi} [1 2])
  (meta ^{:t 'hi} (Term. 1 2))
  (< nil 0)
  )
