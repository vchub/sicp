(ns sicp.ch3.go-block
  (:require [clojure.test :refer :all]
            [clojure.core.async :as a :refer [<! >! <!! >!! go chan timeout]]))

(defn receive-n "chan -> [num]"
  [c n]
  (loop [i 0 acc []]
    (if (= i n)
      acc
      (recur (inc i) (conj acc (<!! c))))))

(defn launch-n-blocks "num -> [num]"
  [n]
  (let [c (chan)]
    (dotimes [i n]
      (go (Thread/sleep 10) (>! c i)))
    (receive-n c n)))

(deftest test-launch-n-blocks
  (testing
   (let [n 4]
     (is (= (set (range n)) (set (launch-n-blocks n)))))))

(test-launch-n-blocks)

(comment
  (/ 7.78 32)
  (/ 8. 48)
  (/ 5.48 32)
  (/ 8.62 5)
  )
