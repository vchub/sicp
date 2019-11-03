(ns problems.probab.graph-path
  (:require [clojure.test :refer :all]))

(defn win-seq
  "num -> [0,1]
   games to win -> path"
  [n]
  (if (zero? n) 1 2)
  )

(testing
  (is (= [[0],[1]] (win-seq 1))) )
