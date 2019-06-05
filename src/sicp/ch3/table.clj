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
(defn insert-in!
  "tbl, [k] -> nil"
  [tbl ks v]
  (loop [tbl tbl ks ks]
    (if (empty? (rest ks))
      (insert! tbl (first ks) v)
      (let [k (first ks)
            tbl-nxt (lookup tbl k)]
        ;; check if it's a table. Use P?
        (if (p/P? tbl-nxt)
          (recur  tbl-nxt (rest ks))
          (let [tbl-nxt (make-table k)]
            (insert! tbl k tbl-nxt)
            (recur tbl-nxt (rest ks))))))))

(defn lookup-in!
  "tbl, [k] -> v"
  [tbl ks]
  (loop [tbl tbl ks ks]
    (cond
      (nil? tbl) nil
      (empty? (rest ks)) (lookup tbl (first ks))
      :else (recur (lookup tbl (first ks)) (rest ks)))))

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
   (is (nil? (lookup-in! t ['a])))
   (insert-in! t ['a] 1)
   (is (= 1 (lookup-in! t ['a])))
   ;; (is (= 1 (or nil (make-table 'a))))
   (insert-in! t ['a 'b] 1)
   (is (= 1 (lookup-in! t ['a 'b])))
   (insert-in! t [1 2 3] 4)
   (is (= 4 (lookup-in! t [1 2 3])))
   ))

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

(testing
 (let [h (fn [p] (get p 0))
       h! (fn [p v] (assoc! p 0 v))
       t (fn [p] (get p 1))
       t! (fn [p v] (assoc! p 1 v))
       p1 (transient [1 nil])
       p2 (transient [2 nil])]
   (is (= 1 (h p1)))
   (h! p1 3)
   (is (= 3 (h p1)))
   (t! p2 p1)
   (is (= 2 (h p2)))
   (is (= 3 (h (t p2))))
    ;; (prn p2)
    ;; (prn (persistent! p2))
   ))
