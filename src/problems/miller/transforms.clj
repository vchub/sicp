(ns problems.miller.transforms
  (:require [clojure.test :refer :all]))

(deftest test-transform
  (testing "misc"
    (is (= 1 1))))

(test-transform)

(defn f "doc string"
  [x]
  (let [x 2
        y 3]
    (+ x y)))

(defn non-blank? [s]
  (not (clojure.string/blank? s)))
