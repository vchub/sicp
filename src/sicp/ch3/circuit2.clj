(ns sicp.ch3.circuit2
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

(defn run-firsts-acts!
  "run first of :acts, remove them and set a new current time"
  [agenda]
  (if-let [[t acts] (get-first agenda)]
    (do (set-time! agenda t)
        (remove-first! agenda)
        (doseq [act acts] (act)))))

(defn propagate!
  "run all agenda :acts, return the last time"
  [agenda]
  (while (not (empty? (get-first agenda)))
    (run-firsts-acts! agenda))
  (let [t (get-time agenda)]
    ;; (set-time! agenda 0)
    t))

(def agenda (make-agenda))

(defn after-delay [dt f] (fn [] (conj-act! agenda f dt)))

(defn b-not [x] {:pre [(#{0 1} x)]} (bit-flip x 0))

(defrecord Wire [state actions])

(defn nil-f [])
(defn make-wire [] (atom (Wire. 0 nil)))
(defn sig [w] (:state @w))
(defn sig! [w x] (when (not= (sig w) x)
                   (swap! w assoc :state x)
                   (doseq [act (:actions @w)]
                     (act))))

(defn add-act! [w f] (swap! w update :actions conj f)
  (f))

;; DELAYS
(def inverter-delay 1)
(def and-delay 2)
(def or-delay 3)

(defn inverter [in out]
  (letfn [(act [] (sig! out (b-not (sig in))))]
    ;; (act)
    (add-act! in (after-delay inverter-delay act))))

(defn and-gate [a b out]
  (letfn [(act [] (sig! out (bit-and (sig a) (sig b))))]
    ;; (act)
    (add-act! a (after-delay and-delay act))
    (add-act! b (after-delay and-delay act))))

(defn or-gate [a b out]
  (letfn [(act [] (sig! out (bit-or (sig a) (sig b))))]
    ;; (act)
    (add-act! a (after-delay or-delay act))
    (add-act! b (after-delay or-delay act))))

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

(defn ripple-carry [as bs ss c-out]
  (loop [as as bs bs ss ss
         cs (concat (map (fn [x] (make-wire)) (range (- (count as) 1))) [c-out])
         c (make-wire)]
    (when-not (empty? as)
      ;; (prn as)
      ;; (prn (count as) (count bs) (count cs))
      (adder (first as) (first bs) c (first ss) (first cs))
      (recur (rest as) (rest bs) (rest ss) (rest cs) (first cs)))))

(defn make-wire-vec [n] (vec (map (fn [x] (make-wire)) (range n))))

(defn int->bit-vec [n vec-len]
  (loop [n n acc []]
    (if (zero? n)
      (if (< (count acc) vec-len)
        (concat (repeat (- vec-len (count acc)) 0) acc)
        acc)
      (recur (bit-shift-right n 1) (cons (bit-and n 1) acc)))))

(defn set-wires!  "[wire], [bit] -> nil"
  [ws xs]
  (vec (map (fn [w b] (sig! w b)) ws (reverse xs))))

(defn prob-wires  "[wire] -> [bit]"
  [ws]
  (map (fn [w] (sig w)) ws))

(defn wires-vec->int [ws] (reduce (fn [acc x] (+ x (* 2 acc))) ws))

(defn small-int-adder "8-bit ripple-carry adder" [x y]
  (let [n 8
        as (make-wire-vec n)
        bs (make-wire-vec n)
        ss (make-wire-vec n)
        c-out (make-wire)
        gate (ripple-carry as bs ss c-out)
        t0 (get-time agenda)
        xs (int->bit-vec x n)
        ys (int->bit-vec y n)]

    (set-wires! as xs)
    (set-wires! bs ys)
    (propagate! agenda)
    ;; (prn "t0" t0)
    ;; (prn "t1" (get-time agenda))
    (->> (reverse (prob-wires ss))
         (wires-vec->int))))

(deftest test-adders
  (testing "small-int-adder"
    (is (= 2 (small-int-adder 1 1)))
    (is (= 3 (small-int-adder 2 1)))
    (is (= 17 (small-int-adder 2 15))))

  (testing "int->bit-vec"
    (is (= [0 1] (int->bit-vec 1 2)))
    (is (= [0 0 1 1] (int->bit-vec 3 4))))

  (testing "ripple-carry"
    (let [n 2
          as (make-wire-vec n)
          bs (make-wire-vec n)
          ss (make-wire-vec n)
          c-out (make-wire)
          gate (ripple-carry as bs ss c-out)
          t0 (get-time agenda)]

      (sig! (nth as 0) 1)
      (propagate! agenda)
      (is (= 1 (sig (nth ss 0))))
      (is (= 0 (sig c-out)))
      (is (= (+ and-delay inverter-delay and-delay) (- (get-time agenda) t0)))

      (sig! (nth bs 0) 1)
      (propagate! agenda)
      (is (= 0 (sig (nth ss 0))))
      (is (= 1 (sig (nth ss 1))))
      (is (= 0 (sig c-out)))
      (is (= (* 5 (+ and-delay inverter-delay and-delay)) (- (get-time agenda) t0)))

      (sig! (nth as 1) 1)
      (propagate! agenda)
      (is (= 0 (sig (nth ss 0))))
      (is (= 0 (sig (nth ss 1))))
      (is (= 1 (sig c-out)))
      (is (= (* 6 (+ and-delay inverter-delay and-delay)) (- (get-time agenda) t0)))))

  (testing "adder"
    (let [a (make-wire)
          b (make-wire)
          c-in (make-wire)
          s (make-wire)
          c-out (make-wire)
          gate (adder a b c-in s c-out)
          t0 (get-time agenda)]

      (sig! a 1)
      (propagate! agenda)
      (is (= 1 (sig s)))
      (is (= 0 (sig c-out)))
      (is (= (+ and-delay inverter-delay and-delay) (- (get-time agenda) t0)))

      (sig! b 1)
      (propagate! agenda)
      (is (= 0 (sig s)))
      (is (= 1 (sig c-out)))

      (sig! c-in 1)
      (propagate! agenda)
      (is (= 1 (sig s)))
      (is (= 1 (sig c-out)))))

  (testing "half-adder"
    (let [a (make-wire)
          b (make-wire)
          c (make-wire)
          s (make-wire)
          gate (half-adder a b s c)
          t0 (get-time agenda)]
      (is (= 0 (sig c)))
      (is (= 0 (sig s)))

      (sig! a 1)
      (propagate! agenda)
      (is (= 1 (sig s)))
      (is (= 0 (sig c)))
      (is (= (+ and-delay inverter-delay and-delay) (- (get-time agenda) t0)))

      (sig! b 1)
      (propagate! agenda)
      (is (= 0 (sig s)))
      (is (= 1 (sig c)))

      (sig! a 0)
      (propagate! agenda)
      (is (= 1 (sig s)))
      (is (= 0 (sig c))))))

(test-adders)

(deftest run-test

  (testing "or-gate"
    (let [a (make-wire)
          b (make-wire)
          out (make-wire)
          gate (or-gate a b out)
          t0 (get-time agenda)]
      (is (= 0 (sig a)))
      (is (= 0 (sig b)))
      (is (= 0 (sig out)))

      (sig! a 1)
      (propagate! agenda)
      (is (= 1 (sig out)))
      (is (= or-delay (- (get-time agenda) t0)))

      (sig! b 1)
      (propagate! agenda)
      (is (= 1 (sig out)))
      (is (= (* 2 or-delay) (- (get-time agenda) t0)))

      (sig! b 0)
      (propagate! agenda)
      (is (= 1 (sig out)))
      (sig! a 0)
      (propagate! agenda)
      (is (= 0 (sig out)))))

  (testing "and-gate"
    (let [a (make-wire)
          b (make-wire)
          out (make-wire)
          gate (and-gate a b out)
          t0 (get-time agenda)]
      (is (= 0 (sig a)))
      (is (= 0 (sig b)))
      (is (= 0 (sig out)))
      (is (= 0 (- (get-time agenda) t0)))

      (sig! a 1)
      (propagate! agenda)
      (is (= 0 (sig out)))
      (is (= and-delay (- (get-time agenda) t0)))

      (sig! b 1)
      (propagate! agenda)
      (is (= (* 2 and-delay) (- (get-time agenda) t0)))
      (is (= 1 (sig out)))

      (sig! a 0)
      (sig! b 1)
      (sig! a 1)
      (sig! b 0)
      (propagate! agenda)
      (is (= 0 (sig out)))))

  (testing "inverter"
    (let [a (make-wire)
          b (make-wire)
          inv (inverter a b)
          t0 (get-time agenda)]
      (is (= 0 (sig a)))
      (is (= 0 (sig b)))
      (let [t0 (get-time agenda)]
        (propagate! agenda)
        (is (= 1 (sig b)))
        (is (= 1 (- (get-time agenda) t0))))

      (sig! a 1)
      (propagate! agenda)
      (is (= 0 (sig b)))
      (is (= 2 (- (get-time agenda) t0)))

      (sig! a 0)
      (propagate! agenda)
      (is (= 1 (sig b)))
      (is (= 3 (- (get-time agenda) t0)))))

  (testing "wire"
    (let [a (make-wire)
          cnt (atom 0)
          act (fn [] (swap! cnt inc))
          delayed-act (after-delay 1 act)]
      (add-act! a act)
      (is (= 0 (sig a)))
      (is (= 1 @cnt))
      (sig! a 1)
      (is (= 1 (sig a)))
      (is (= 2 @cnt))

      (sig! a 1)
      (is (= 1 (sig a)))
      (is (= 2 @cnt))

      (sig! a 0)
      (is (= 0 (sig a)))
      (is (= 3 @cnt))

      (add-act! a delayed-act)
      (sig! a 1)
      (is (= 1 (sig a)))
      (propagate! agenda)
      (is (= 6 @cnt)))))

(run-test)

(deftest test-agenda

  (testing "agenda"
    (let [ag (make-agenda)
          cnt (atom 0)]
      (conj-act! ag (fn [] (swap! cnt inc)) 1)
      (conj-act! ag (fn [] (swap! cnt inc)) 1)
      (conj-act! ag (fn [] (swap! cnt inc)) 4)
      (propagate! ag)
      (is (= 3 @cnt))
      (is (= 4 (get-time ag)))
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

(test-agenda)

(comment
  (first (sorted-map 1 2))
  (first (sorted-map))
  (let [[x y] nil] [x y])
  (doseq [a [1 2] b [3 4]]
    (prn a b))
  (map (fn [x y] (prn x y) [x y]) [1 2] [3 4]))
