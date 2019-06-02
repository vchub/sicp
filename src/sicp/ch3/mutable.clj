(ns sicp.ch3.mutable
  (:require [clojure.test :refer :all]))

(defprotocol Pair
  (h [p] "content accessor")
  (h! [_ h] "content set")
  (t [p] "next accessor")
  (t! [p t] "t set")
  (last-pair [p])
  (add [p h])
  (to-list [p])
  (append! [x y])
  (rev [p]))

(deftype P [^:volatile-mutable h- ^:volatile-mutable t-]
  Pair
  (h [_] h-)
  (h! [_ h] (set! h- h))
  (t [p] t-)
  (t! [_ t] (set! t- t))
  (last-pair [p] (loop [p p]
                   (if (nil? (t p))
                     p
                     (recur (t p)))))
  (add [p h] (t! (last-pair p) (P. h nil)))
  (to-list [p] (cond
                 (nil? (t p)) (list (h p))
                 :else (conj (to-list (t p)) (h p))))

  (append! [p y] (t! (last-pair p) y))

  (rev [p] (loop [x p y nil]
             (if (nil? x)
               y
               (let [t (t x)]
                 (t! x y)
                 (recur t  x))))))

(defn P? [x] (= P (type x)))

(defn count-pairs [l]
  {:pre [(P? l)]}
  (loop [st [l] visited #{} cnt 0]
    (cond
      (empty? st) cnt
      :else
      (let [[x & st] st
            visited (conj visited x)
            st (concat (filter (fn[x] (and (not(visited x)) (P? x))) [(h x) (t x)]) st)]
        (recur st visited (inc cnt))))))

(defn cns [h p] (P. h p))
(defn flip [f] (fn [& args] (apply f (reverse args))))

(testing
  (let [l1 (P. 1 (P. 2 nil))
        l2 (P. 2 l1)
        l3 (P. l1 l1)
        ]
    (is (= [1 2] (to-list l1)))
    (is (= 2 (count-pairs l1)))
    (is (= 3 (count-pairs l2)))
    (is (= 4 (count-pairs l3)))
    (t! (t l1) l3)
    (is (= 4 (count-pairs l3)))
    )
  )

(testing
  (is (not= (P. 1 nil) (P. 1 nil)) )
  (let [p (P. 1 nil)]
    (is (=  p p)))

  (is (= P (type (P. 1 nil))))

 (is (= [2 1 0] (to-list (reduce (flip cns) (P. 0 nil) [1 2]))))

  (let [p (cns 2 (cns 1 nil))
        l2 (cns 3 (cns 4 nil))]
    (is (= [2 1] (to-list p)))
    (append! p nil)
    (is (= [2 1] (to-list p)))
    (append! p l2)
    (is (= [2 1 3 4] (to-list p)))

    (is (= (reverse [2 1 3 4]) (to-list (rev p)))))

  )
