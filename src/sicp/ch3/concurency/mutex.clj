(ns sicp.ch3.concurency.mutex
  (:require [clojure.test :refer :all])
  (:import (java.util.concurrent Semaphore)))

(defprotocol IMutex
  (acquire [_])
  (release [_]))

(defrecord Mutex [x dt]
  IMutex
  (acquire [_] (if @x
                 (do (Thread/sleep dt) (recur))
                 (reset! x true)))
  (release [_] (reset! x false)))

(defn mutex [dt] (Mutex. (atom false) dt))

(defn serial [dt]
  (let [mut (mutex dt)]
    (fn [f]
      (fn [& args]
        (acquire mut)
        (let [ret (apply f args)]
          (release mut)
          ret)))))

(defn part-on-pred
  [xs pred sym]
  (loop [xs xs acc []]
    (cond
      (empty? xs) [acc xs]
      (pred (first xs)) (recur (rest xs) (conj acc [(first xs) sym]))
      :else [acc xs])))

(defn odd-even-threads "[num] -> [[odd t] [even f]] accumulate in 2 Thread."
  [xs]
  (let [acc (atom [])
        ;; mut (Semaphore. 1)
        mut (mutex 1)
        reducer (fn [pred sym] (loop [xs xs]
                                 (cond
                                   (empty? xs) xs
                                   (pred (first xs)) (do
                                                       (.acquire mut)
                                                       (let [[a xs] (part-on-pred xs pred sym)]
                                                         ;; (prn a xs @acc)
                                                         (swap! acc concat a)
                                                         (.release mut)
                                                         (Thread/sleep 1)
                                                         (recur xs)))
                                   :else (do
                                           (let [[a xs] (part-on-pred xs (complement pred) 'ok)]
                                             (Thread/sleep 1)
                                             (recur xs))))))
        acc-odd (fn [] (reducer odd? true))
        acc-even (fn [] (reducer even? false))
        t1 (Thread. acc-odd)
        t2 (Thread. acc-even)]

    (.start t1)
    (.start t2)
    (.join t1)
    (.join t2)
    @acc))

(deftest test-odd-even-threads

  (testing "serial"
    (let [s (serial 10)
          acc (atom [])
          f10 (fn []  (Thread/sleep 10)
                (swap! acc conj 10))
          f1 (fn []  (Thread/sleep 1)
               (swap! acc conj 1))
          f10s (s f10)
          f1s (s f1)

          t10 (Thread. f10)
          t1 (Thread. f1)
          t10s (Thread. f10s)
          t1s (Thread. f1s)]

      (.start t10)
      (.start t1)
      (.join t10)
      (.join t1)
      (is (= [1 10] @acc))

      (reset! acc [])
      (.start t10s)
      (.start t1s)
      (.join t10s)
      (.join t1s)
      ;; (is (= [10 1] @acc))
      ))

  (testing "mutex"
    (let [mut (mutex 1)
          acc (atom [])
          t10 (Thread. (fn []
                         (Thread/sleep 10)
                         (acquire mut)
                         (swap! acc conj 10)
                         (release mut)
                         (Thread/sleep 1)
                         (acquire mut)
                         (swap! acc conj 11)
                         (release mut)
                         ))
          t1 (Thread. (fn []
                        (Thread/sleep 1)
                        (acquire mut)
                        (swap! acc conj 1)
                        (release mut)
                        (Thread/sleep 11)
                        (acquire mut)
                        (swap! acc conj 2)
                        (release mut)
                        ))]
      (.start t10)
      (.start t1)
      (.join t10)
      (.join t1)
      (is (= [1 10 11 2] @acc))))

  (testing "acc-on-pred"
    (is (= [[[1 true]] [2 3 4]] (part-on-pred (range 1 5) odd? true)))
    (is (= [[[1 true] [3 true]] [4 5]] (part-on-pred [1 3 4 5] odd? true))))

  (let [n 4]
    (is (= (map #(vector % (odd? %)) (range n)) (odd-even-threads (range n))))))

(test-odd-even-threads)

(defn two-threads []
  (let [mutex (Semaphore. 1)
        t1 (fn []
             (prn "t1 start")
             (.acquire mutex)
             (Thread/sleep 10)
             (prn "t1 middle")
             (.release mutex)
             (prn "t1 done"))
        t2 (fn []
             (prn "t2 start")
             (Thread/sleep 5)
             (.acquire mutex)
             (prn "t2 middle")
             (.release mutex)
             (prn "t2 done"))]
    (.start (Thread. t1))
    (.start (Thread. t2))))

;; (two-threads)
