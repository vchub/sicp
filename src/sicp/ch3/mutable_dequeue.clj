(ns sicp.ch3.mutable-dequeue
  (:require [clojure.test :refer :all]
            [sicp.ch3.mutable :as p :refer [t t! h h!]])
  (:import [sicp.ch3.mutable P]))

;; Element is (P. content (P. f-pointer r-pointer))
(defn content [e] (h e))
(defn f-pointer [e] (h (t e)))
(defn set-f-pointer! [e x] (h! (t e) x))
(defn r-pointer [e] (t (t e)))
(defn set-r-pointer! [e x] (t! (t e) x))

(defn repr [e] [(content e) (f-pointer e) (r-pointer e)])

(defn empty-q? [q] (nil? (h q)))

(defn insert-r! [q x] (let [n (P. x (P. nil nil))]
                        (if (empty-q? q)
                          (do (h! q n) (t! q n))
                          (do
                            (set-f-pointer! n (t q))
                            (set-r-pointer! (t q) n)
                            (t! q n)))))

(defn insert-f! [q x] (let [n (P. x (P. nil nil))]
                        (if (empty-q? q)
                          (do (h! q n) (t! q n))
                          (do
                            (set-f-pointer! (h q) n)
                            (set-r-pointer! n (h q))
                            (h! q n)))))
(defn to-list [l]
  (loop [e (h l) acc []]
    ;; (prn e acc)
    (if (nil? e)
      acc
      (recur (r-pointer e) (conj acc (content e))))))

(defn delete-r! [q] (if (= (h q) (t q))
                      (do
                        (h! q nil)
                        (t! q nil))
                      (do
                        (t! q (f-pointer (t q)))
                        (set-r-pointer! (t q) nil))))

(defn delete-f! [q] (if (= (h q) (t q))
                      (do
                        (h! q nil)
                        (t! q nil))
                      (do
                        (h! q (r-pointer (h q)))
                        (set-f-pointer! (h q) nil)
                        (prn (repr (h q))))))

(defn front [q] (when (not (empty-q? q)) (content (h q))))
(defn rear [q] (when (not (empty-q? q)) (content (t q))))

(defn make-queue
  ([] (P. nil nil))
  ([& xs] (let [q (make-queue)]
            (doseq [x xs]
              (insert-r! q x))
            q)))

(testing
 (let [q1 (make-queue 1)
       q2 (make-queue 1 2 3)]
   (is (= [1] (to-list q1)))
   (is (= [1 2 3] (to-list q2)))
   (is (nil? (front (make-queue))))
   (is (nil? (rear (make-queue))))
   (is (= 1 (front q1)))
   (is (= 1 (rear q1)))
   (is (= 1 (front q2)))
   (is (= 3 (rear q2)))
   (insert-r! q1 2)
   (is (= [1 2] (to-list q1)))
   (delete-r! q1)
   (is (= [1] (to-list q1)))
   (delete-r! q1)
   (is (= [] (to-list q1)))
   (is (empty-q? q1))

   (is (= [1 2 3] (to-list q2)))
   (delete-r! q2)
   (is (= [1 2] (to-list q2)))

   (delete-f! q2)
   (is (= [2] (to-list q2)))
   (insert-f! q2 1)
   (is (= [1 2] (to-list q2)))
   (insert-f! q2 3)
   (is (= [3 1 2] (to-list q2)))))
