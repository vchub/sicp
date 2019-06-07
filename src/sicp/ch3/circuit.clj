(ns sicp.ch3.circuit
  (:require [clojure.test :refer :all]
            [clojure.core.async :as a :refer [<! >! <!! >!! go chan timeout]])
  (:import (java.util.concurrent ScheduledThreadPoolExecutor TimeUnit)))

(defn after-delay [dt f] (fn [] (go
                                  (<! (timeout dt))
                                  (f))))

(defn b-not [x] {:pre [(#{0 1} x)]} (bit-flip x 0))

(defrecord Wire [state action])

(defn nil-f [])
(defn make-wire [] (atom (Wire. 0 nil-f)))
(defn sig [w] (:state @w))
;; (defn sig! [w x] (when (not= (sig w) x)
;;                    (swap! w assoc :state x)
;;                    ((:action @w))))
(defn sig! [w x] (swap! w assoc :state x)
  ((:action @w)))

(defn add-act! [w f] (swap! w assoc :action f))

;; DELAYS
(def inverter-delay 1)
(def and-delay 1)
(def or-delay 1)

(defn inverter [in out]
  (letfn [(act [] (sig! out (b-not (sig in))))]
    (act)
    (add-act! in (after-delay inverter-delay act))))

(defn and-gate [a b out]
  (letfn [(act [] (sig! out (bit-and (sig a) (sig b))))]
    (act)
    (add-act! a (after-delay and-delay act))
    (add-act! b (after-delay and-delay act))))

(defn or-gate [a b out]
  (letfn [(act [] (sig! out (bit-or (sig a) (sig b))))]
    (act)
    (add-act! a (after-delay and-delay act))
    (add-act! b (after-delay and-delay act))))

(deftest run-test

  (testing "or-gate"
    (let [a (make-wire)
          b (make-wire)
          out (make-wire)
          gate (or-gate a b out)]
      (is (= 0 (sig a)))
      (is (= 0 (sig b)))
      (is (= 0 (sig out)))

      (sig! a 1)
      (Thread/sleep (* 2 inverter-delay))
      (is (= 1 (sig out)))

      (sig! b 1)
      (Thread/sleep (* 2 inverter-delay))
      (is (= 1 (sig out)))

      (sig! b 0)
      (Thread/sleep (* 2 inverter-delay))
      (is (= 1 (sig out)))
      (sig! a 0)
      (Thread/sleep (* 2 inverter-delay))
      (is (= 0 (sig out)))))

  (testing "and-gate"
    (let [a (make-wire)
          b (make-wire)
          out (make-wire)
          gate (and-gate a b out)]
      (is (= 0 (sig a)))
      (is (= 0 (sig b)))
      (is (= 0 (sig out)))

      (sig! a 1)
      (Thread/sleep (* 2 inverter-delay))
      (is (= 0 (sig out)))

      (sig! b 1)
      (Thread/sleep (* 2 inverter-delay))
      (is (= 1 (sig out)))))

  (testing "inverter"
    (let [a (make-wire)
          b (make-wire)
          inv (inverter a b)]
      (is (= 0 (sig a)))
      (is (= 1 (sig b)))
      (Thread/sleep (* 2 inverter-delay))
      (is (= 1 (sig b)))

      (sig! a 1)
      ;; (is (= 0 (sig b)))
      (Thread/sleep (* 2 inverter-delay))
      (is (= 0 (sig b)))

      (sig! a 0)
      ;; (is (= 0 (sig b)))
      (Thread/sleep (* 2 inverter-delay))
      (is (= 1 (sig b)))))

  (testing "wire"
    (let [a (make-wire)
          cnt (atom 0)
          act (fn [] (swap! cnt inc))
          delayed-act (after-delay 1 act)]
      (add-act! a act)
      (is (= 0 (sig a)))
      (is (= 0 @cnt))
      (sig! a 1)
      (is (= 1 (sig a)))
      (is (= 1 @cnt))
      (sig! a 1)
      (is (= 1 (sig a)))
      (is (= 2 @cnt))
      ;; (sig! a 0)
      ;; (is (= 0 (sig a)))
      ;; (is (= 2 @cnt))
      (add-act! a delayed-act)
      (sig! a 1)
      (is (= 1 (sig a)))
      ;; (is (= 2 @cnt))
      ;; (after-delay 100 (fn[]
      ;;                    (is (= 1 2))))
      (Thread/sleep 2)
      (is (= 3 @cnt))))

  (testing "bitwise"
    (are [x y] (= x y)
      0 (bit-and 0 0)
      0 (bit-and 1 0)
      1 (bit-and 1 1)
      1 (bit-or 0 1)
      -1 (bit-not 0)
      -2 (bit-not 1)
      1 (or 1 0)
      0 (or 0 1)
      1 (b-not 0)
      0 (b-not 1))
    (is (thrown? AssertionError (b-not 3)))))

(run-test)

(comment
  (#{1 2} 0))
