(ns sicp.ch3.constraints1
  (:require [clojure.test :refer :all]))

;; Box (constraints) protocol
(defn inform-got-val [b] (b 'got-val))
(defn inform-no-val [b] (b 'no-val))

(defn wire []
  (let [-v (atom nil)
        -owner (atom nil)
        -constraints (atom [])
        inform-others (fn [msg owner] (doseq [b (filter #(not= % owner) @-constraints)]
                                        (condp = msg
                                          'got-val (inform-got-val b)
                                          'no-val (inform-no-val b))))
        ;; Wire protocol
        get-v (fn [] @-v)
        has-v? (fn [] (some? @-v))
        set-v! (fn [v owner] (condp = (get-v)
                               nil (do
                                     (reset! -v v)
                                     (reset! -owner owner)
                                     (inform-others 'got-val owner)
                                     'ok)
                               v 'ok
                               (throw (Exception. (format "Contradiction -v %s v %s" (get-v) v)))))

        forget-v! (fn [owner] (when (= @-owner owner)
                                (reset! -v nil)
                                (inform-others 'no-val owner)
                                'ok))

        connect (fn [box] (swap! -constraints conj box))
        dispatch (fn [msg] (condp = msg
                             'get-v get-v
                             'has-v? has-v?
                             'set-v! set-v!
                             'forget-v! forget-v!
                             'connect connect))]

    dispatch))

(defn get-v [w] ((w 'get-v)))
(defn has-v? [w] ((w 'has-v?)))
(defn set-v! [w v owner] ((w 'set-v!) v owner))
(defn forget-v! [w owner] ((w 'forget-v!) owner))
(defn connect [w box] ((w 'connect) box))

(deftest test-wire
  (testing "dispatch"
    (let [w (wire)]
      (is (= false ((w 'has-v?))))
      (is (= nil (get-v w)))
      (is (thrown? IllegalArgumentException (w 'has-xxx)))
      (set-v! w 1 'user)
      (is (= 1 (get-v w)))
      (is (= true (has-v? w)))
      (set-v! w 1 'user)
      (is (thrown? Exception (set-v! w 2 'user)))
      (is (nil? (forget-v! w 'foo)))
      (is (= 1 (get-v w)))
      (is (= 'ok (forget-v! w 'user)))
      (is (= false ((w 'has-v?))))
      (is (= nil (get-v w))))))

(test-wire)
