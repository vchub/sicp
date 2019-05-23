(ns sicp.ch2.hoffman
  (:require [clojure.test :refer :all]
            [clojure.data :as dat]
            [clojure.set :as s]))

(testing
 (let [q (java.util.PriorityQueue. (fn [x y] (compare  y x)))
       n 5]
   (doseq [i (range n)]
     (.add q i))
   (is (= (reverse (range n)) (map (fn [x] (.poll q)) (range n))))))

(defprotocol Huf
  (in? [this x])
  (c [this])
  (leaf? [x]))

(defrecord Leaf [c w]
  Huf
  (in? [this x] ((:c this) x))
  (c [this] (first (:c this)))
  (leaf? [x] true))

(defrecord Node [l r c w]
  Huf
  (in? [this x] ((:c this) x))
  (leaf? [x] false))

(defn merge-nodes
  "Node, Node -> Node"
  [x y]
  (let [[x y] (if (< (:w x) (:w y))
                [y x]
                [x y])]
    (Node. x y (s/union (:c y) (:c x)) (+ (:w y) (:w x)))))

(defn freq->q [xs]
  "[[symbol freq]] -> java.util.PriorityQueue."
  (let [q (java.util.PriorityQueue. (fn [x y] (compare (:w x) (:w y))))]
    (doseq [x xs]
      (.add q (Leaf. #{(first x)} (second x))))
    q))

(defn make-huff-tree
  "[[symbol freq]] -> Node"
  [xs]
  (let [q (freq->q xs)]
    (loop [x (.poll q) y (.poll q)]
      (if (nil? y)
        x
        (do
          (.add q (merge-nodes x y))
          (recur (.poll q) (.poll q)))))))

(defn encode
  "symbol -> [{\0,\1}]"
  [x t]
  (loop [t t acc []]
    (if (leaf? t)
      acc
      (cond
        (in? (:l t) x) (recur (:l t) (conj acc \0))
        (in? (:r t) x) (recur (:r t) (conj acc \1))
        :else (throw (Exception. (str "no symbol " x)))))))

(defn encode-seq
  "[symbol] -> [{0,1}]"
  [xs t]
  (clojure.string/join (mapcat #(encode % t) xs)))

(defn decode-seq
  " [{0,1}]->[symbol]"
  [xs t]
  (loop [xs xs node t acc []]
    (cond
      (empty? xs) acc
      (leaf? node) (recur xs t (conj acc (c node)))
      (= \0 (first xs)) (recur (rest xs) (:l t) acc)
      (= \1 (first xs)) (recur (rest xs) (:r t) acc)
      :else (throw (Exception. (str "bad bit " (first xs)))))))

(defn to-seq [q]
  (map (fn [x] (c (.poll q))) (range (count q))))

(testing
 (let [q (freq->q '((a 3) (c 0) (b 2)))]
   (is (= 'c (c (.peek q))))
   (is (= '(c b a) (to-seq q))))
  (let [t (make-huff-tree '((\a 3) (\c 1) (\b 2)))]
  ;; (prn t)
    (is (not (leaf? t)))
    (is (leaf? (:l t)))
    (is (= \a (c (:l t))))
    (is (= [\0] (encode \a t)))
    (is (= "010" (encode-seq "ab" t)))
    (is (= "011" (encode-seq "ac" t)))
    (is (= "ac" (decode-seq "011" t)))
    (is (= "acb" (decode-seq "01110" t)))
    (is (= "acab" (decode-seq "011010" t)))))

(comment
  (let [s #{1}]
    (first s)
    (s 1))
  (make-huff-tree '((a 1) (b 2)))
  (clojure.string/join [\a \b]))
