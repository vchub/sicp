(ns problems.probab.graph-path
  (:require [clojure.test :refer :all]))

(defn win-seq
  "num -> [[0,1]]
   games to win -> paths"
  [n]
  (letfn [(go [path a b]
              (if (or (zero? a) (zero? b))
                [path]
                (let [pa (go (conj path 0) (dec a) b)
                      pb (go (conj path 1) a (dec b))
                      ret (concat pa pb)]
                ;; (prn ret)
                  ret)))]
    (count (go [] n n))))

(testing
 (is (= 2 (win-seq 1)))
  (is (= 6 (win-seq 2)))
  (is (= 20 (win-seq 3)))
  )

(comment
  (* 3 4))
