(ns sicp.ch2.poly.poly
  (:require [clojure.test :refer :all]
            [sicp.ch2.poly.arith :as arith :refer [put apply-genric]]
            [clojure.set :as s]))

(defn install-poly []
  (letfn [(sq[x] (* x x))
          (real[z] (first z))
          (img[z] (second z))
          (make[r i] ^{:t 'complex} [(real r) (real i)])
          (magn[z] (Math/sqrt (+ (sq (real z)) (sq (img z)))))
          (angle[z] (Math/atan (/ (real z) (img z))))
          (mul[x y] (let [m (* (magn x) (magn y))
                          a (+ (angle x) (angle y))
                          r (* m (Math/cos a))
                          i (* m (Math/sin a))]
                      (make r i)))
          ]
    (put 'real '(complex) real)
    (put 'img '(complex) img)
    (put 'make '(real real) make)
    (put 'magn '(complex) magn)
    (put 'angle '(complex) angle)
    (put 'mul '(complex complex) mul)
    'done
    ))

(install-poly)

(testing
  (let [z (arith/complex 1 2)]
    (is (= 1 (arith/real z)))
    (is (= 2 (arith/img z)))
    (is (= 5.0 (arith/magn (arith/complex 3 4))))
    ))

;; (swap! f-tbl assoc :b 1)
;; (testing
;;   (is (= 0 (:t @f-tbl)))
;;   (is (= 1 (:b @f-tbl)))
;;   (is (= 1 (:b @arith/f-tbl)))
;;   )
