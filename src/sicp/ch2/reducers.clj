(ns sicp.ch2.reducers
  (:require [clojure.test :refer :all]
            [clojure.math.combinatorics :as comb]))

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
  (is (= '(22 26 30) (accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12))))))

(defn dot-product
  [xs ys]
  (reduce + 0 (map * xs ys)))

(defn matrix*vector
  [m ys]
  (map (partial dot-product ys) m))

(defn transpose
  [xs]
  (if (empty? (first xs))
    nil
    (cons (map first xs)
          (transpose (map rest xs)))))

(defn transpose1
  [xs]
  (accumulate-n (fn [acc x] (cons x acc)) nil (reverse xs)))

(defn matrix*matrix
  [xs ys]
  (map (partial matrix*vector (transpose ys)) xs))

(testing
 (is (= '(2 1) (reduce #(cons %2 %1) nil '(1 2))))
  (is (= 2 (dot-product '(1 1) '(1 1))))
  (is (= 7 (dot-product '(1 3) '(1 2))))
  (is (= '(7 6) (matrix*vector '((1 3) (2 2)) '(1 2))))
  (is (= '((7 7) (5 8)) (transpose '((7 5) (7 8)))))
  (is (= '((7 7) (5 8)) (transpose1 '((7 5) (7 8)))))
  (is (= (transpose '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
         (transpose '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))))
  (is (= '((7 6) (6 8)) (matrix*matrix '((1 3) (2 2)) '((1 3) (2 1)))))
  (is (= '((26 18) (51 31)) (matrix*matrix '((4 2) (3 5)) '((2 2) (9 5))))))

(def foldl reduce)

(defn foldr
  [op init xs]
  (reduce (fn [acc x] (op x acc)) init (reverse xs)))

(testing
 (is (= '(2 1) (foldl #(cons %2 %1) nil '(1 2))))
  (is (= '(1 2) (foldr #(cons %1 %2) nil '(1 2))))

  (is (= 1/6 (foldl / 1 '(1 2 3))))
  (is (= 3/2 (foldr / 1 '(1 2 3))))

  (is (= '(((nil 1) 2) 3) (foldl list  nil '(1 2 3))))
  (is (= '(1 (2 (3 nil))) (foldr list  nil '(1 2 3)))))

(defn reverse1 [xs]
  (foldl #(cons %2 %1) nil xs))

(defn reverse2 [xs]
  (foldr #(concat %2 (list %1)) nil xs))

(testing
 (is (= '(2 1) (reverse1 '(1 2))))
  (is (= '(2 1) (reverse2 '(1 2)))))

(defn permx [x xs]
  (loop [h [] t xs acc []]
    (if (empty? t)
      (cons (conj h x) acc)
      (recur (conj h (first t)) (rest t) (cons (concat h [x] t) acc)))))

(def perm (memoize (fn [xs]
  (if (empty? xs)
    [[]]
    (mapcat (fn [s] (permx (first xs) s)) (perm (rest xs)))))))


(def permutations(memoize (fn [xs]
  (if (empty? xs)
    [[]]
    (mapcat (fn[x]
              (map (fn[p] (cons x p)) (permutations (remove (fn[y] (= x y)) xs)))) xs)))))

(testing
 (is (= [[0 1 2] [1 0 2] [1 2 0]] (reverse (permx 0 [1 2]))))
  (is (= (set '((0 1 2) (0 2 1) (1 0 2) (1 2 0) (2 0 1) (2 1 0))) (set (perm [0 1 2]))))
  (is (= (set (comb/permutations [1 2 3 4])) (set (perm [1 2 3 4]))))

  (is (= (set (comb/permutations [1 2 3])) (set (permutations [1 2 3]))))
  )

(comment
  (let [N 10]
    (prn "perm insert " (time (do (perm (range N)) nil)))
    (prn "perm remove " (time (do (permutations (range N)) nil)))
    )
  (remove (fn[x] (= 1 x)) '(0 1 2))
  (for [i (range 3)
        j (range i)]
    (list j i))
  (mapcat identity '((1 (2)) (3 4)))
  (flatten '((1 (2)) (3 4)))
  (map first '((1 2) (3 4)))
  (map rest '((1 2) (3 4)))
  (map rest '(() ()))
  (map rest '()))
