(ns sicp.ch3.table-func
  (:require [clojure.test :refer :all]))

(defn h [v] (nth v 0))
(defn t [v] (nth v 1))
(defn nxt [v] (subvec v 1))

(defn make-table []
  (let [kvs (atom (transient []))
        ;; k -> int
        indexof (fn [k] (loop [kvs @kvs i 0]
                          (cond
                            (zero? (count kvs)) nil
                            (= k (h (h kvs))) i
                            :else (recur (nxt kvs) (inc i)))))
        ;; k -> [k v]
        lookup (fn [k] (if-let [i (indexof k)]
                         (t (nth @kvs i))))

        insert! (fn [k v] (if-let [i (indexof k)]
                            (swap! kvs assoc! i [k v])
                            (swap! kvs conj! [k v])))
        dispatch (fn [msg] (condp = msg
                             'lookup lookup
                             'insert! insert!))]
    dispatch))

(defn lookup [tbl k] ((tbl 'lookup) k))
(defn insert! [tbl k v] ((tbl 'insert!) k v))

(testing
 (let [t (make-table)]
   (is (= nil (lookup t 1)))
   ;; (insert! t 2 2)
   (insert! t 1 2)
   ;; (is (= 2 (lookup t 2)))
   ;; (insert! t 1 3)
   ;; (is (= 3 (lookup t 1)))
   ;;
   ;; (insert! t 1 2)
   ;; (insert! t 1 5)

   ;; (insert! t 2 5)
   ;; (insert! t 2 3)
   ;; (insert! t 4 1)
   ;; (is (= 3 (lookup t 1)))
   ))

(comment
  (take 1 [1 2 3])
  (.indexOf [1 2 3] 2)
  (.indexOf [1 2 'x 'x] 'x)
  (take 1 (transient [1 2 3]))
  (subvec 1 (transient [1 2 3]))
  )
