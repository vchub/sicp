(ns sicp.ch3.table
  (:require [clojure.test :refer :all]
            [sicp.ch3.mutable :as p :refer [t t! h h!]])
  (:import [sicp.ch3.mutable P]))

(defn make-record [k v nxt] (P. (P. k v) nxt))
(defn rkey [r] (h (h r)))
(defn rval [r] (h (t r)))
(defn make-table [k] (P. k nil))

(defn find-r [l k] (cond
                     (nil? l) nil
                     (= k (rkey l)) (h l)
                     :else (recur (t l) k)))

(defn lookup [tbl k] (let [r (find-r (t tbl) k)]
                       (if (not (nil? r))
                         (t r)
                         nil)))

(defn insert! [tbl k v] (let [r (find-r (t tbl) k)]
                          (cond
                            (nil? r) (let [new-r (make-record k v (t tbl))]
                                       (t! tbl new-r))
                            (not= v (t r)) (t! r v)
                            :else nil)))

(defn find-parent-rec
  "table, key -> Pair
   k = (rkey (nxt r))"
  [tbl k]
  (cond
    (nil? (t tbl)) nil
    (= k (rkey (t tbl))) tbl
    :else (recur (t tbl) k)))

(defn del! [tbl k] (if-let [parent (find-parent-rec tbl k)]
                     (t! parent (t (t parent)))))

(testing
 (let [t (make-table 'x)]
   (is (nil? (lookup t 'a)))
   (insert! t 'a 1)
   (is (= 1 (lookup t 'a)))
   (is (nil? (lookup t 2)))
   (insert! t 2 3)
   (is (= 3 (lookup t 2)))
   (del! t 2)
   (is (nil? (lookup t 2)))
   (insert! t 2 3)
   (del! t 'a)
   (is (nil? (lookup t 'a)))))
