(ns sicp.ch3.account
  (:require [clojure.test :refer :all]))

(defrecord Account [b w d])

(defn make-account
  "num -> [->num, f, f]
   f: (num)->num"
  [balance]
  (let [state (atom balance)
        bal (fn[] @state)
        withdraw (fn withdraw [x] (if (<= x @state)
                                    (do
                                    (swap! state - x)
                                    @state)
                                    "not enough funds"))
        deposit (fn[x] (swap! state + x))]
    (Account. bal withdraw deposit)))

(testing
  (let [a (make-account 10)]
    (is (= 10 ((:b a))))
    (is (= 5 ((:w a) 5)))
    (is (= "not enough funds" ((:w a) 6)))
    (is (= 15 ((:d a) 10)))
    (is (= 0 ((:w a) 15)))
    ))

(comment
  (defrecord T[t s])
  (meta ^{:t :hi} 's)
  (meta (with-meta 's {:t :hi}))
  (meta (with-meta (T. 1 2) {:t :hi}))
  )
