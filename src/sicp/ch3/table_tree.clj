(ns sicp.ch3.table-tree
  (:require [clojure.test :refer :all]))

(defrecord Pair [h t])

(defn make-p [h t] (atom (Pair. h t)))
(defn pair? [p] (and (= (type p) clojure.lang.Atom) (= (type @p) Pair)))
(defn h [p] (:h @p))
(defn h! [p x] (swap! assoc :h x))
(defn t [p] (:t @p))
(defn t! [p x] (swap! p assoc :t x))
(defn to-list [p] (loop [p p acc []]
                    (cond
                      (nil? p) acc
                      (not (pair? p)) (conj acc p)
                      :else (recur (t p) (conj acc (h p))))))

(defrecord Node [k v l r]
  java.lang.Comparable
  (compareTo [x y] (compare k (:k y))))

(defn make-table [k] (make-p k nil))
(defn make-node [k v l r] (atom (Node. k v l r)))
(defn node? [p] (and (= (type p) clojure.lang.Atom) (= (type @p) Node)))
(defn nkey [n] (:k @n))
(defn nval [n] (:v @n))
(defn nval! [n v] (swap! n assoc :v v))
(defn l [n] (:l @n))
(defn r [n] (:r @n))
(defn l! [n x] (swap! n assoc :l x))
(defn r! [n x] (swap! n assoc :r x))
(defn cmp [node k] (compare (nkey node) k))

(defn find-node [node k] (cond
                           (nil? node) nil
                           (pos? (cmp node k)) (recur (l node) k)
                           (neg? (cmp node k)) (recur (r node) k)
                           :else node))

(defn lookup [tbl k] (if-let [node (find-node (t tbl) k)] (nval node)))

(defn insert! [tbl k v]
  (if (nil? (t tbl))
    (t! tbl (make-node k v nil nil))
    (loop [node (t tbl)]
      (cond
        (pos? (cmp node k)) (if (nil? (l node))
                              (let [n1 (make-node k v nil nil)]
                                (l! node n1))
                              (recur (l node)))
        (neg? (cmp node k)) (if (nil? (r node))
                              (let [n1 (make-node k v nil nil)]
                                (r! node n1))
                              (recur (r node)))
        :else (nval! node v)))))

(defn inorder "tbl -> [[k v]]"
  [tbl]
  (if-let [n (t tbl)]
    (loop [st [n] acc []]
      (if (empty? st)
        acc
        (let [[n & st] st]
          ;; (prn "n" n "st" st "acc" acc)
          (cond
            (nil? n) (recur st acc)
            (not (node? n)) (recur st (conj acc n))
            :else (recur (into st [(r n) [(nkey n) (nval n)] (l n)]) acc)))))
    []))

(testing
 (let [tbl (make-table 'a)]
   (is (nil? (lookup tbl 1)))
   (insert! tbl 1 2)
   (insert! tbl 2 3)
   (insert! tbl 0 0)
   (is (= 2 (lookup tbl 1)))
   (is (= 3 (lookup tbl 2)))
   (is (= 0 (lookup tbl 0)))
   ;; (is (= [[1 2] [2 3] ] (inorder tbl)))
   (insert! tbl 4 3)
   (is (= 3 (lookup tbl 4)))

   (is (= [[0 0] [1 2] [2 3] [4 3]] (inorder tbl)))

   (is (node? (make-node 1 2 nil nil)))))

(testing
 (let [x (Node. 1 2 nil nil)
       y (Node. 1 2 nil nil)]
   (is (= 0 (compare x y)))
   (is (= -1 (compare x (Node. 2 2 x x))))

   (is (= 0 (cmp (atom x) 1)))
   (is (= 1 (cmp (atom x) 0)))
   (is (= -1 (cmp (atom x) 2)))))

(testing
 (let [p1 (make-p 1 nil)
       p2 (make-p 2 nil)]
   (t! p1 p2)
   (is (pair? p1))
   (is (= [1 2] (to-list p1)))
   (is (= 1 (h p1)))
   (is (= 2 (h (t p1))))))

(comment
  (into '( 1 2 ) [3])
  (into [1 2] [3])
  (into [1 2] [5 'x [3]])
  (peek (into [1 2] [[3]]))
  (concat [1 2] [[3]])
  (let [p (make-p 1 nil)]
    (type p)
    (type @p))
  (type Pair)
  (assoc [1 2] 1 3))
