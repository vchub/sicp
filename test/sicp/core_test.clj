(ns sicp.core-test
  (:require [clojure.test :refer :all]
            [sicp.core :refer :all]))

(testing
  (are [x y] (= x y)
       2 (+ 1 1)
       3 (- 4 1)
       ))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 1 1))))
