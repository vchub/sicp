(ns sicp.ch3.mutable-queue
  (:require [clojure.test :refer :all]
            [sicp.ch3.mutable :as p :refer [t t! h h!]])
  (:import [sicp.ch3.mutable P]))

(defn empty-q? [q] (nil? (h q)))

(defn insert! [q x] (let [n (P. x nil)]
                      (if (empty-q? q)
                        (do (h! q n) (t! q n))
                        (do
                          (t! (t q) n)
                          (t! q n)))))

(defn delete! [q] (when (not (empty-q? q)) (h! q (t (h q)))))
(defn front [q] (h q))

(defn make-queue
  ([] (P. nil nil))
  ([& xs] (let [q (make-queue)]
            (doseq [x xs]
              (insert! q x))
            q)))

(testing
 (let [q1 (make-queue 1)
       q2 (make-queue 1 2 3)]
   (is (p/eq? (P. 1 nil) (h q1)))
   (is (p/eq? (P. 1 nil) (h q1)))
   (is (= [1] (p/to-list (h q1))))
   (is (= [1 2 3] (p/to-list (h q2))))
   (delete! q2)
   (is (= [2 3] (p/to-list (h q2))))
   (insert! q2 4)
   (is (= [2 3 4] (p/to-list (h q2))))
   (delete! q1)
   (is (empty-q? q1))))
