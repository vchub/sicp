(ns sicp.ch3.chanels
  (:require [clojure.test :refer :all]
            [clojure.core.async :as a :refer [<! >! <!! >!! go go-loop chan timeout]])
  (:import (java.util.concurrent ScheduledThreadPoolExecutor TimeUnit)))

;; (defn after-delay [dt f] (a/thread (Thread/sleep dt) (f)))

(defn after-delay [dt f] (go
                           (<! (timeout dt))
                           (f)))

(defn after-delay-ex [dt f]
  (.schedule (ScheduledThreadPoolExecutor. 1) f dt TimeUnit/MILLISECONDS))

(defn nums [out]
  ;; (assert (= 2 1) "assert message")
  (a/go-loop [i 0]
             ;; (prn "i:" i)
    (>! out i)
    (recur (inc i))))

(defn concurent-nums [n]
  (let [c (chan)]
    (nums c)
    (loop [i 0 acc []]
      ;; (prn i acc)
      (if (= n i)
        (do
          (a/close! c)
          acc)
        (recur (inc i) (conj acc (<!! c)))))))

(defn primes-chan "-> chan of primes" []
  (let [integers (chan)
       primes (chan)]
    (go-loop [i 2] (>! integers i) (recur (inc i)))
    (go-loop [cur-ch integers]
             (let [prime (<! cur-ch)
                   new-ch (chan 1 (filter #(pos? (mod % prime))))]
               (>! primes prime)
               (a/pipe cur-ch new-ch)
               (recur new-ch))
             )
    primes
    ))

(deftest test-primes
  (testing "primes"
    (let [pc (primes-chan)]
      (is (= 2 (<!! pc)))
      (is (= 3 (<!! pc)))
      (is (= 5 (<!! pc)))
      (is (= [7 11 13 17] (<!! (a/into [] (a/take 4 pc)))))
      (is (= [0 1 2 3] (<!! (a/into [] (a/to-chan (range 4))))))
      )))

(test-primes)

(deftest run-test
  (testing "after-delay-ex"
    (let [f (fn [msg] (prn "in f-ex" msg))]
      (prn "before delay ex")
      (after-delay-ex 100 #(f "hi"))
      (prn "after delay ex")))

  (testing "after-delay"
    (let [f (fn [msg] (prn "in f" msg))]
      (prn "before delay")
      (after-delay 1000 #(f "hi"))
      (prn "after delay")))

  (testing "concurent-nums"
    (is (= (range 5) (concurent-nums 5))))

  (testing "chanel"
    (let [c (chan)
          uc (fn [in] (let [out (chan)]
                        (a/go-loop [] (>! out (clojure.string/upper-case (<! in))) (recur))
                        out))]
    ;; (>!! c "foo")
      (go (>! c "foo"))
      (is (= "FOO" (<!! (uc c)))))

    (testing "timeout"
      (let [c (chan)]

      (a/take! (go 1) (fn[x] (prn "from go" x)))
      (is (= 1 (<!! (go 1))))

      (go (<! (timeout 40)) (>! c 4))
      (go (<! (timeout 1)) (>! c 1))
      (is (= 1 (<!! c)))
      (is (= 4 (<!! c)))
      (a/close! c)
      )
      )

    (let [c (chan 10)]
      (>!! c 'hi)
      (is (= 'hi (<!! c)))
      (a/close! c))
    (let [c (chan)]
      (a/thread (>!! c 'hi))
      (is (= 'hi (<!! c)))
      (a/close! c))
    (let [c (chan)]
      (go (>! c 'hi))
      (is (= 'hi (<!! c)))
      (a/close! c))
    (let [c (chan)]
      (go (>! c 'hi))
      (is (= 'hi (<!! (go (<! c)))))
      (a/close! c))

    (let [n 1000
          cs (repeatedly n chan)
          begin (System/currentTimeMillis)]
      (doseq [c cs] (go (>! c "hi")))
      (dotimes [i n]
        (let [[v c] (a/alts!! cs)]
          (assert (= "hi" v))))
      (println "Read" n "msgs in" (- (System/currentTimeMillis) begin) "ms"))))

;; (run-test)

(comment
  (delay 1)
  (after-delay 1 (fn [] (prn 'done))))
