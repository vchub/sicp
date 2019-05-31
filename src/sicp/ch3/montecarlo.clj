(ns sicp.ch3.montecarlo
  (:require [clojure.test :refer :all]))

(defn abs [x] (if (< 0 x) x (* -1 x)))
(defn close-to [a b dx] (< (abs (- a b)) dx))
(defn log [x] (prn x) x)

(def gcd (memoize (fn gcd [a b] (if (zero? b) a (recur b (rem a b))))))

(defn pi
  "6/π2 is the probability that two integers chosen at random will have no factors in common"
  [trials N]
  (->> (map (fn [x] (gcd (rand-int N) (rand-int N))) (range trials))
       (filter #(= 1 %))
       (reduce +)
       ((fn [x] (Math/sqrt (/ 6 (/ x trials)))))))

(testing
 (is (close-to Math/PI (pi 100 100) 0.1))
  (is (close-to Math/PI 3.14 0.1))
  (is (= 1 (gcd 1 1)))
  (is (= 1 (gcd 2 3)))
  (is (= 1 (gcd 7 3)))
  (is (= 3 (gcd 9 3)))
  (is (= 1/2 (->> 6
                  ((fn [x] (/ 3 x)))))))
