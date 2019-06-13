(ns sicp.ch3.concurency.bank
  (:require [clojure.test :refer :all]
            [clojure.math.combinatorics :as comb]))

(defn ex3_4 []
  (let [b 100
        procs [(fn [b] (+ b 10)) (fn [b] (- b 20)) (fn [b] (/ b 2))]
        xform (fn [fs] (reduce #(%2 %1) b fs))]
    (map xform (comb/permutations procs))))

(testing "ex3.4"
  (is (= (sort [35 40 45 50]) (sort (set (ex3_4))))))

(testing "volatile"
  (let [x (volatile! 0)]
    (vswap! x inc)
    (is (= 1 @x))
    ))

(comment
  (comb/permutations [1 2]))
