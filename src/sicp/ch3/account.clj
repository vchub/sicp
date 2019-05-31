(ns sicp.ch3.account
  (:require [clojure.test :refer :all]))

(defrecord Account [b w d])

(defn make-account0
  "num -> [->num, f, f]
   f: (num)->num"
  [balance]
  (let [state (atom balance)
        bal (fn [] @state)
        withdraw (fn withdraw [x] (if (<= x @state)
                                    (do
                                      (swap! state - x)
                                      @state)
                                    "not enough funds"))
        deposit (fn [x] (swap! state + x))]
    (Account. bal withdraw deposit)))

(testing
 (let [a (make-account0 10)]
   (is (= 10 ((:b a))))
   (is (= 5 ((:w a) 5)))
   (is (= "not enough funds" ((:w a) 6)))
   (is (= 15 ((:d a) 10)))
   (is (= 0 ((:w a) 15)))))

(defn make-monitored
  "fn -> fn
  (f arg) = (fn arg)
  (f 'how-many-calls?) -> num of calls"
  [f]
  (let [cnt (atom 0)]
    (fn [arg]
      (if (= arg 'how-many-calls?)
        @cnt
        (do (swap! cnt inc)
            (f arg))))))

(testing
 (let [sq (make-monitored #(* % %))]
   (is (= 0 (sq 'how-many-calls?)))
   (is (= 4 (sq 2)))
   (is (= 1 (sq 'how-many-calls?)))
   (is (= 1 (sq 'how-many-calls?)))))

(defn make-account-
  "psw, balance -> psw, message -> num"
  [psw b]
  (let [cnt (atom 0)
        balance (fn [] @b)
        withdraw (fn [x] (if (<= x @b)
                           (do (swap! b - x)
                               @b)
                           "insufficient funds"))
        deposit (fn [x] (swap! b + x) @b)

        call-cops (fn [] (prn "call-cops"))

        check-psw (fn [ps] (if (= psw ps)
                             (do (swap! cnt (fn [x] 0)) true)
                             (do (swap! cnt inc)
                                 (when (> @cnt 2) (call-cops))
                                 false)))

        join-acc (fn [ps] (make-account- ps b))

        dispatch (fn [ps message]
                   (if (check-psw ps)
                     (condp = message
                       'balance balance
                       'withdraw withdraw
                       'deposit deposit
                       'join-acc join-acc
                       :else (throw (Exception. (str "unknown request MAKE-ACCOUNT message " message))))
                     (fn [x] "Incorrect password")))]
    dispatch))

(defn make-account
  "psw, balance -> psw, message -> num"
  [psw b]
(make-account- psw (atom b))
  )

(defn make-joint-acc
  [acc psw new-psw]
  ((acc psw 'join-acc) new-psw))

(testing
 (let [a (make-account 'pass 5)]
   (is (= 4 ((a 'pass 'withdraw) 1)))
   (is (= "Incorrect password" ((a 'x 'withdraw) 1)))
   (is (= "Incorrect password" ((a 'x 'withdraw) 1)))
    ;; (is (= "Incorrect password" ((a 'x 'withdraw) 1)))
   (is (= 3 ((a 'pass 'withdraw) 1)))
   (is (= 6 ((a 'pass 'deposit) 3)))
   ;; joint account
   (let [b (make-joint-acc a 'pass 'new-psw)]

     (is (= 6 ((a 'pass 'balance))))
     (is (= 5 ((b 'new-psw 'withdraw) 1)))
     (is (= 5 ((a 'pass 'balance)))))))

(comment
  (defrecord T [t s])
  (meta ^{:t :hi} 's)
  (meta (with-meta 's {:t :hi}))
  (meta (with-meta (T. 1 2) {:t :hi})))

;; tranducers
(def xform (comp (map inc) (filter even?)))

(testing
 (is (= [2 4] (into [] xform (range 4))))
  (is (= [2 4] (sequence xform (range 4))))
  (is (= 6 (transduce xform + (range 4))))
  (is (= 7 (transduce xform + 1 (range 4)))))
