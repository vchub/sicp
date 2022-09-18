(ns sicp.ch41.lazy-test
  (:require [clojure.test :refer :all]
            [sicp.ch41.lazy :refer :all]))

(deftest thuds
  (testing "unless"
    (is (= 1 (evall '(unless false 1 2))))
    (is (= 1 (evall '(unless false 1 (/ 1 0)))))
    (is (= 2 (evall '(unless true (/ 1 0) 2)))))

  (testing "primitives"
    (is (= 'x (evall '(def x (+ 1 1)))))))
