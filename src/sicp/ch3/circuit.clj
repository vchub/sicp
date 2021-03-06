(ns sicp.ch3.circuit
  (:require [clojure.test :refer :all]
            [clojure.core.async :as a :refer [<! >! <!! >!! go chan timeout]]))

(defn make-agenda [] (atom {:now 0 :acts (sorted-map)}))
(defn get-time [agenda] (:now @agenda))
(defn set-time! [agenda t] (swap! agenda assoc :now t))
(defn get-first "agenda -> [time [act]]"
  [agenda]
  (first (:acts @agenda)))

(defn remove-first! [agenda] (swap! agenda update :acts dissoc (first (get-first agenda))))

;; it's after-delay
(defn conj-act! [agenda act dt]
  (swap! agenda update :acts update (+ (get-time agenda) dt) (fnil conj []) act))

(defn propagate!
  "run first of :acts, remove them and set a new current time"
  [agenda]
  (let [[t acts] (get-first agenda)]
    (doseq [act acts] (act))
    (set-time! agenda t)
    (remove-first! agenda)))

(deftest agenda

  (testing "agenda"
    (let [ag (make-agenda)
          cnt (atom 0)]
      (conj-act! ag (fn [] (swap! cnt inc)) 1)
      (conj-act! ag (fn [] (swap! cnt inc)) 1)
      (conj-act! ag (fn [] (swap! cnt inc)) 4)
      (propagate! ag)
      (is (= 2 @cnt))
      (is (= 1 (get-time ag)))
      (propagate! ag)
      (is (= 3 @cnt))
      (is (= 4 (get-time ag))))

    (let [ag (make-agenda)]
      (is (= nil (get-first ag)))
      (conj-act! ag (constantly 1) 1)
      (conj-act! ag (constantly 2) 1)
      (conj-act! ag (constantly 5) 3)
      (let [[t acts] (get-first ag)]
        (is (= 1 t))
        (is (= 2 (count acts)))
        (is (= 1 ((first acts))))
        (is (= 2 ((second acts)))))
      (remove-first! ag)
      (is (= 3 (first (get-first ag))))))

  (testing "map"
    (let [m (sorted-map)
          m (update m 0 (fnil conj []) 1)]
      (is (= [1] (get m 0)))
      (is (= nil (get (dissoc m (first (first m))) 0))))))

(agenda)

(defn after-delay [dt f] (fn [] (go
                                  (<! (timeout dt))
                                  (f))))

(defn b-not [x] {:pre [(#{0 1} x)]} (bit-flip x 0))

(defrecord Wire [state actions])

(defn nil-f [])
(defn make-wire [] (atom (Wire. 0 nil)))
(defn sig [w] (:state @w))
(defn sig! [w x] (when (not= (sig w) x)
                   (swap! w assoc :state x)
                   (doseq [act (:actions @w)]
                     (act))))
;; (defn sig! [w x] (swap! w assoc :state x)
;;   ((:action @w)))

(defn add-act! [w f] (swap! w update :actions conj f))

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

(defn half-adder [a b s c]
  (let [d (make-wire)
        e (make-wire)]
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)))

(defn adder [a b c-in s c-out]
  (let [c1 (make-wire)
        c2 (make-wire)
        s1 (make-wire)]
    (half-adder b c-in s1 c1)
    (half-adder a s1 s c2)
    (or-gate c1 c2 c-out)))

(deftest test-adders
  (testing "adder"
    (let [a (make-wire)
          b (make-wire)
          c-in (make-wire)
          s (make-wire)
          c-out (make-wire)
          gate (adder a b c-in s c-out)]

      (sig! a 1)
      (Thread/sleep (* 5 and-delay))
      (is (= 1 (sig s)))
      (is (= 0 (sig c-out)))

      (sig! b 1)
      (Thread/sleep (* 8 and-delay))
      (is (= 0 (sig s)))
      (is (= 1 (sig c-out)))

      (sig! c-in 1)
      (Thread/sleep (* 10 and-delay))
      (is (= 1 (sig s)))
      (is (= 1 (sig c-out)))))

  (testing "half-adder"
    (let [a (make-wire)
          b (make-wire)
          c (make-wire)
          s (make-wire)
          gate (half-adder a b s c)]
      (is (= 0 (sig c)))
      (is (= 0 (sig s)))

      (sig! a 1)
      ;; (sig! b 0)
      (Thread/sleep (* 3 and-delay))
      (is (= 1 (sig s)))
      (is (= 0 (sig c)))

      (sig! b 1)
      (Thread/sleep (* 4 and-delay))
      (is (= 0 (sig s)))
      (is (= 1 (sig c)))

      (sig! a 0)
      (Thread/sleep (* 4 and-delay))
      (is (= 1 (sig s)))
      (is (= 0 (sig c))))))

(test-adders)

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
      (Thread/sleep (* 3 inverter-delay))
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
      (is (= 1 @cnt))

      (sig! a 0)
      (is (= 0 (sig a)))
      (is (= 2 @cnt))

      (add-act! a delayed-act)
      (sig! a 1)
      (is (= 1 (sig a)))
      ;; (is (= 2 @cnt))
      ;; (after-delay 100 (fn[]
      ;;                    (is (= 1 2))))
      (Thread/sleep 2)
      (is (= 4 @cnt))))

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
  (let [m (sorted-map 3 4 1 2 2 1)]
    (first m))
  (let [m {3 4 1 2 2 1}]
    (first m))
  (type Double)
  (class Double)
  (type (Double. 1.0))
  (= (type 2.0) (type (Double. 1.0)))
  (doto (java.util.HashMap.)
    (.put 'a 1)
    (.put 'b 2)
    (println))

  (.. (java.util.Date.) getTime toString)
  ((constantly 5))
  (#{1 2} 0))
