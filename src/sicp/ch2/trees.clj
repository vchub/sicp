(ns sicp.ch2.trees
  (:require [clojure.test :refer :all]))

(defn leaf? [x] (number? x))

(defn deep-reverse [xs]
  (loop [xs xs acc nil]
    (cond
      (empty? xs) acc
      (leaf? (first xs)) (recur (rest xs) (cons (first xs) acc))
      :else (recur (rest xs) (cons (deep-reverse (first xs)) acc)))))

(testing
 (is (= '((4 3) 2 1) (deep-reverse '(1 2 (3 4)))))
  (is (= `((4 3) 2 1 (0)) (deep-reverse `((0) 1 2 (3 4)))))
  (is (seq? (list 1)))
  (is (not (seq? [1])))
  ;; (is (seq? [1]))
  (is (= [1 2 3] (concat `(1 2) `(3))))
  (is (= '((3) 1 2) (conj `(1 2) `(3))))
  (is (= (list `(3) 1 2) (conj `(1 2) `(3))))
  (is (= [1 2 [3]] (conj [1 2] [3])))
  (is (= [1 2] (conj [1] 2))))

(defn fringe
  [xs]
  (loop [xs xs acc []]
    (cond
      (empty? xs) acc
      (leaf? (first xs)) (recur (rest xs) (conj acc (first xs)))
      :else (recur (concat (first xs) (rest xs)) acc))))

(testing
 (is (= [1 2 3] (fringe [1 2 3])))
  (is (= [1 2 3] (fringe [1 '(2 3)])))
  (is (= [1 2 3 4] (fringe [1 '(2 3) [4]]))))

(defn leaf? [t] (number? (:s t)))
(defn branch? [t] (not (nil? (:s t))))

(defn weight
  [t]
  (cond
    (leaf? t) (:s t)
    (branch? t) (weight (:s t))
    :else (+ (weight (:l t)) (weight (:r t)))))

(defn torque [b] (if (branch? b)
                   (float (* (:len b) (weight b)))
                   (throw (Exception. (str "not branch " b)))))

(defn balanced? [t]
  (cond
    (leaf? t) true
    (branch? t) (balanced? (:s t))
    :else
    (and
     (= (torque (:l t)) (torque (:r t)))
     (and (balanced? (:l t))
          (balanced? (:r t))))))

(testing
 (let [t0 {:l {:len 1 :s 4} :r {:len 1 :s {:l {:len 1 :s 4} :r {:len 2 :s 2}}}}
       t1 {:l {:len 1.5 :s 4} :r {:len 1 :s {:l {:len 1 :s 4} :r {:len 2 :s 2}}}}]
   (is (= 4 (weight (:l t0))))
   (is (= 10 (weight t0)))
   (is (not (balanced? t0)))
   (is (balanced? (get-in t1 [:r :s])))
   (is (not= 6 6.0))
   (is (balanced? t1)))
  (is (empty? nil)))

(defn map-tree
  [f t]
  (cond
    (empty? t) t
    (list? (first t)) (cons (map-tree f (first t)) (map-tree  f (rest t)))
    :else (cons (f (first t)) (map-tree f (rest t)))))

(defn square-tree [t]
  (map (fn[x] (if (list? x)
                (square-tree x)
                (* x x))) t))

(defn map-t [f t]
  (map (fn[x] (if (list? x)
                (map-t f x)
                (f x))) t))

(defn square [x] (* x x))

(testing
 (is (= '(2 3) (map-tree inc '(1 2))))
 (is (= '(2 (3 4)) (map-tree inc '(1 (2 3)))))
 (is (= '(1 (4 9)) (square-tree '(1 (2 3)))))
 (is (= '(1 (4 9)) (map-t square '(1 (2 3)))))
 (is (= [1 4] (into [] (map #(* % %)) '(1 2))))
 (is (= [1 4] (sequence (map #(* % %)) '(1 2))))
 )


(defn subsets [xs]
  (if (empty? xs)
    '(())
    (let [x (first xs)
          s (subsets (rest xs))]
      ;; (prn x s)
      (concat s (map #(cons x %) s))))
  )

(testing
  (is (= '(() (1)) (subsets '(1))))
  (is (= (set '(() (1) (2) (1 2))) (set (subsets '(1 2)))))
  )




(comment
  (let [f (map inc)]
    (take 3 (f [1 2])))
  (:l {:l 1 :r 2})
  (:s {:l 1 :r 2})
  (:s nil)
  (not nil))
