(ns sicp.ch3.mutable
  (:require [clojure.test :refer :all]))

(defprotocol Pair
  (c [p] "content accessor")
  (c! [_ c] "content set")
  (nx [p] "next accessor")
  (nx! [p nx] "nx set")
  (last-pair [p])
  (add [p c])
  (to-list [p])
  (append! [x y])
  (rev [p]))

(deftype P [^:volatile-mutable c- ^:volatile-mutable nx-]
  Pair
  (c [_] c-)
  (c! [_ c] (set! c- c))
  (nx [p] nx-)
  (nx! [_ nx] (set! nx- nx))
  (last-pair [p] (loop [p p]
                   (if (nil? (nx p))
                     p
                     (recur (nx p)))))
  (add [p c] (nx! (last-pair p) (P. c nil)))
  (to-list [p] (cond
                 (nil? (nx p)) (list (c p))
                 :else (conj (to-list (nx p)) (c p))))

  (append! [p y] (nx! (last-pair p) y))

  (rev [p] (loop [x p y nil]
             (if (nil? x)
               y
               (let [t (nx x)]
                 (nx! x y)
                 (recur t  x))))))

(defn cns [c p] (P. c p))
(defn flip [f] (fn [& args] (apply f (reverse args))))

(testing
 (is (= [2 1 0] (to-list (reduce (flip cns) (P. 0 nil) [1 2]))))

  (let [p (cns 2 (cns 1 nil))
        l2 (cns 3 (cns 4 nil))]
    (is (= [2 1] (to-list p)))
    (append! p nil)
    (is (= [2 1] (to-list p)))
    (append! p l2)
    (is (= [2 1 3 4] (to-list p)))

    (is (= (reverse [2 1 3 4]) (to-list (rev p)))))

  (let [p (P. 1 nil)]
    (is (= 1 (.c p)))
    (add p 2)
    (is (= 2 (.c (.nx p))))
    (is (= [1 2] (to-list p)))))
