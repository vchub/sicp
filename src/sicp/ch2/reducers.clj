(ns sicp.ch2.reducers
  (:require [clojure.test :refer :all]))

(defn horner
  "(…(anx+an−1)x+⋯+a1)x+a0."
  [as x]
  (+ (first as) (reduce (fn [acc a] (* (+ acc a) x)) 0 (reverse (rest as)))))

(defn horner2
  "(…(anx+an−1)x+⋯+a1)x+a0."
  [as x]
  (reduce (fn [acc a] (+ (* acc x) a)) 0 (reverse as)))

(testing
 (is (= 1 (horner '(1 1) 0)))
  (is (= 3 (horner '(1 1) 2)))
  (is (= 9 (horner '(1 0 2) 2)))
  (is (= 79  (horner (list 1 3 0 5 0 1) 2)))
  (is (= 79  (horner2 (list 1 3 0 5 0 1) 2)))
  (is (= 1 (horner2 '(1 1) 0)))
  (is (= [[1 3] [2 4]] (map vector '(1 2) '(3 4))))
  (is (= [[1 3] [2 4]] (apply map vector '((1 2) (3 4))))))

(defn count-leaves [t]
  (reduce + 0 (map (fn [x]
                     (if (list? x)
                       (count-leaves x)
                       1))
                   t)))

(testing
 (is (= 2 (count-leaves '(1 2))))
  (is (= 2 (count-leaves '((1) (2)))))
  (is (= 4 (count-leaves '(() (1) (2 3) (4))))))


(defn accumulate-n [op init seqs]
  (if (empty? (first seqs))
    nil
    (cons (do
            (let [r (map first seqs)]
            ;; (prn r)
            (reduce op init r)))
          (do
            (let [r (map rest seqs)]
            ;; (prn r)
            (accumulate-n op init r))))))

(testing
  (is (= '(4 6) (accumulate-n + 0 '((1 2) (3 4)))))
  (is (= '(22 26 30) (accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))))
  )

(comment
  (map first '((1 2) (3 4)))
  (map rest '((1 2) (3 4)))
  (map rest '(() ()))
  (map rest '())
  )
