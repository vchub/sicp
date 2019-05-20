(ns sicp.ch2.8q
  (:require [clojure.test :refer :all]))

(defn abs [x] (if (< x 0) (* -1 x) x))

(defn valid? [board]
  (let [[row col] (first board)]
    (empty? (filter (fn [[r c]] (or
                                 (= c col)
                                 (= (abs (- r row)) (abs (- c col)))))
                    (rest board)))))

(def q-h (memoize (fn [board row rows]
                   (if (<= rows row)
                     (do
                         ;; (prn board row rows)
                       1)
                     (reduce + 0 (map (fn [c]
                                        (let [board (cons [row c] board)]
                                          (if (valid? board)
                                            (q-h board (inc row) rows)
                                            0)))
                                      (range rows)))))))
(defn queens [rows]
  (q-h nil 0 rows))

(testing
 (is (= 1 (queens 1)))
  (is (= 0 (queens 2)))
  (is (= 0 (queens 3)))
  (is (= 2 (queens 4)))
  (is (= 10 (queens 5)))
  (is (= 92 (queens 8))))

(comment
  (let [[x y] '(1 2)]
    [x y]))
