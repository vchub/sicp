(ns sicp.ch3.average-dist
  (:require [clojure.test :refer :all]
            [sicp.ch3.streams.infinite :refer [close-enough partial-sums abs] :as inf]))

;; What Is The Distance Between Two Random Points In A Square?
;; https://www.youtube.com/watch?v=i4VqXRRXi68&feature=youtu.be&fbclid=IwAR1QcwaictXbTUn4yMkrtS2U4ayLnRIFbSGuQ_HGjFinrUWGlHoMAKRMO4s

(defn square [x] (* x x))
(defn hypoth [x y] (Math/sqrt (+ (square x) (square y))))
(defn distance [[x1 y1] [x2 y2]]
  (hypoth (- x2 x1) (- y2 y1)))

(defn point2-s []
  (repeatedly #(vector [(rand) (rand)] [(rand) (rand)])))

(defn average-dist [n]
  "n -> num
   average distance between rand points in square [0 1]
   monte-carlo n experiments"
  (->> (take n (point2-s))
       (map #(apply distance %))
       (reduce +)
       ((fn [x] (/ x n)))))

(defn average-s "Stream [num] -> Stream [average of [num]]"
  [xs]
  (letfn [(iter [n acc xs]
            (lazy-seq (cons acc (iter (inc n)
                                      (/ (+ (first xs) (* (dec n) acc)) n)
                                      (drop 1 xs)))))]
    (drop 1 (iter 1 (first xs) (drop 1 xs)))))

(deftest test-dist
  (is (= 5.0 (apply hypoth [3 4])))
  ;; (is (= [] (take 5 (point2-s))))
  (is (= [1 3/2 2 5/2 3] (take 5 (average-s (range)))))
  (is (= 7/2 (nth (average-s (range)) 5)))

  (let [n 1e4
        dt 1e-2]
    (is (close-enough 0.521 (average-dist n) dt))
    (is (close-enough 0.521 (nth (average-s (map #(apply distance %) (point2-s))) n) dt))))

(test-dist)
