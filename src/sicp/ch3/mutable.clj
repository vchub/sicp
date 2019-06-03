(ns sicp.ch3.mutable
  (:require [clojure.test :refer :all]))

(defprotocol Pair
  (h [p] "content accessor")
  (h! [_ h] "content set")
  (t [p] "next accessor")
  (t! [p t] "t set")
  (last-pair [p])
  (add-last! [p h])
  (to-list [p])
  (append! [p y])
  (rev [p]))

;; implemented later
(def P?)

(deftype P [^:volatile-mutable h- ^:volatile-mutable t-]
  Pair
  (h [_] h-)
  (h! [_ h] (set! h- h))
  (t [p] t-)
  (t! [_ t] (set! t- t))
  (last-pair [p] (loop [p p]
                   (if (not (P? (t p)))
                     p
                     (recur (t p)))))
  (add-last! [p h] (t! (last-pair p) (P. h nil)))
  (to-list [p] (cond
                 (nil? (t p)) (list (h p))
                 (not (P? (t p))) (list (h p) (t p))
                 :else (conj (to-list (t p)) (h p))))

  (append! [p y] (t! (last-pair p) y))

  (rev [p] (loop [x p y nil]
             (if (nil? x)
               y
               (let [t (t x)]
                 (t! x y)
                 (recur t  x))))))

(defn P? [x] (= P (type x)))
(defn eq? [x y] (if (and (P? x) (P? y))
                  (and (eq? (h x) (h y)) (eq? (t x) (t y)))
                  (= x y)))

(def PP P)

(defn count-pairs [l]
  {:pre [(P? l)]}
  (loop [st [l] visited #{} cnt 0]
    (cond
      (empty? st) cnt
      :else
      (let [[x & st] st
            visited (conj visited x)
            st (concat (filter (fn [x] (and (not (visited x)) (P? x))) [(h x) (t x)]) st)]
        (recur st visited (inc cnt))))))

(defn no-cycles? [l]
  (let [iter (fn iter [l visited post-v]
               ;; (prn l visited post-v)
               (cond
                 (not (P? l)) true
                 (and (visited l) (not (post-v l))) false
                 (visited l) true
                 :else
                 (let [visited (conj visited l)
                       res (and (iter (h l) visited post-v) (iter (t l) visited post-v))
                       post-v (conj post-v l)]
                   res)))]
    (iter l #{} #{})))

;; Floyd's cycle-finding algorithm.
;; http://en.wikipedia.org/wiki/Cycle_detection#Tortoise_and_hare
(defn cycle? [l]
  (letfn [(run [slow fast]
            (cond
              (or (not (P? slow))
                  (not (P? fast))
                  (not (P? (t fast)))
                  (not (P? (t (t fast))))) false
              (= (h slow) (h fast)) true
              :else (recur (t slow) (t (t fast)))))]
    (run l (t l))))

(defn cns [h p] (P. h p))
(defn flip [f] (fn [& args] (apply f (reverse args))))

(testing
 (let [p (P. 1 2)]
   (P? p)
   (is (eq? (P. 1 2) (last-pair p)))
   (is (eq? (P. 1 (P. 2 nil)) (P. 1 (P. 2 nil))))

   (is (= [1 2] (to-list p))))

  (let [l1 (P. 1 (P. 2 nil))
        l2 (P. 2 l1)
        l3 (P. l1 l1)]
    (is (= [1 2] (to-list l1)))
    (is (= 2 (count-pairs l1)))
    (is (no-cycles? l1))
    (is (= 3 (count-pairs l2)))
    (is (= 4 (count-pairs l3)))
    (is (no-cycles? l2))
    (is (no-cycles? l3))

    (is (not (cycle? l1)))
    (is (not (cycle? l2)))
    (is (not (cycle? l3)))

    (t! (t l1) l3)
    (is (= 4 (count-pairs l3)))
    (is (not (no-cycles? l3)))
    (is (cycle? l3))))

(testing
 (is (not= (P. 1 nil) (P. 1 nil)))
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

    (is (= (reverse [2 1 3 4]) (to-list (rev p))))))
