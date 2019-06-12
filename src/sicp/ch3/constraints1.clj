(ns sicp.ch3.constraints1
  (:require [clojure.test :refer :all]))

;; Box (constraints) protocol
(defn inform-got-val [b] (b 'got-val))
(defn inform-no-val [b] (b 'no-val))

(defn wire []
  (let [-v (atom nil)
        -owner (atom nil)
        -constraints (atom [])
        inform-others (fn [proc owner]
                        (doseq [b (filter #(not= % owner) @-constraints)]
                          (proc b)))
        ;; Wire protocol
        get-v (fn [] @-v)
        has-v? (fn [] (some? @-v))
        set-v! (fn [v owner] (condp = (get-v)
                               nil (do
                                     (reset! -v v)
                                     (reset! -owner owner)
                                     (inform-others inform-got-val owner)
                                     'ok)
                               v 'ok
                               (throw (Exception. (format "Contradiction -v %s v %s" (get-v) v)))))

        forget-v! (fn [owner] (when (= @-owner owner)
                                (reset! -v nil)
                                (inform-others inform-no-val owner)
                                'ok))

        ;; connect (fn [box]
        ;;                     (swap! -constraints conj box)
        ;;                     (when (has-v?) (inform-got-val box) ))

        connect (fn [box] (when-not (contains? @-constraints box)
                            (swap! -constraints conj box)
                            (when (has-v?) (inform-got-val box) )))

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

(defn adder [wa wb wc]
  (let [ws [wa wb wc]]
    (letfn [(got-val []
              (let [[a b c] (map get-v ws)]
                (cond
                  (and (some? a) (some? b)) (set-v! wc (+ a b) me)
                  (and (some? a) (some? c)) (set-v! wb (- c a) me)
                  (and (some? b) (some? c)) (set-v! wa (- c b) me)
                  :else nil)))

            (no-val [] (doseq [w ws] (forget-v! w me))
                    (got-val))

            (me [msg] (condp = msg
                        'got-val (got-val)
                        'no-val (no-val)))]
      (doseq [w ws] (connect w me))
      me)))

(defn const [w c]
  (letfn [(got-val [])
          (no-val [] (set-v! w c))
          (me [msg] (condp = msg
                      'got-val (got-val)
                      'no-val (no-val)))]
    (connect w me)
    (set-v! w c me)
    me))

(defn multiplyer [wa wb wc]
  (let [ws [wa wb wc]]
    (letfn [(got-val []
              (let [[a b c] (map get-v ws)]
                (cond
                  ;; (some #(= 0 %) [a b c]) 0
                  (and (some? a) (some? b)) (set-v! wc (* a b) me)
                  (and (some? a) (some? c)) (set-v! wb (/ c a) me)
                  (and (some? b) (some? c)) (set-v! wa (/ c b) me)
                  :else nil)))

            (no-val [] (doseq [w ws] (forget-v! w me))
                    (got-val))

            (me [msg] (condp = msg
                        'got-val (got-val)
                        'no-val (no-val)))]
      (doseq [w ws] (connect w me))
      me)))

(defn celsius-fahrenheit-converter [c f]
  (let [w (wire)
        u (wire)
        x (wire)
        v (wire)
        y (wire)]
    (multiplyer c w u)
    (multiplyer v x u)
    (adder v y f)
    (const w 9)
    (const x 5)
    (const y 32)
    'ok))

(defn square [c f]
  (let [w (wire)
        ]
    (multiplyer c c f)
    'ok))

(deftest test-wire

  (testing "square"
    (let [c (wire)
          f (wire)
          sq (square c f)]
      (is (= nil (get-v c)))
      (set-v! c 5 'user)
      (is (= 25 (get-v f)))
      (forget-v! c 'user)
      (is (= nil (get-v f)))
      (set-v! f 9 'user)
      (is (= 3 (get-v c)))
      ))

  (testing "Celsius Fahrenheit converter"
    (let [c (wire)
          f (wire)
          cnv (celsius-fahrenheit-converter c f)]
      (is (= nil (get-v c)))
      (is (= nil (get-v f)))
      (set-v! c 25 'user)
      (is (= 77 (get-v f)))
      (forget-v! c 'user)
      (set-v! f 100 'user)
      (is (= 37 (int (get-v c))))
      (forget-v! f 'user)
      (set-v! c 40 'user)
      (is (= 104 (get-v f)))))

  (testing "multiplyer"
    (let [a (wire)
          b (wire)
          c (wire)
          _ (multiplyer a b c)
          _ (const a 2)]
      (set-v! b 3 'user)
      (is (= 6 (get-v c)))
      (forget-v! c 'user)
      (is (= 6 (get-v c)))
      (forget-v! b 'user)
      (is (= nil (get-v c)))
      (set-v! b 4 'user)
      (is (= 8 (get-v c)))))

  (testing "consts and adders"
    (let [a (wire)
          b (wire)
          c (wire)
          d (wire)
          e (wire)
          add1 (adder a b c)
          add2 (adder c d e)
          _ (const a 2)
          _ (const b 3)
          _ (const d 4)]
      (is (= 5 (get-v c)))
      (is (= 9 (get-v e)))
      (forget-v! e 'user)
      (is (= 9 (get-v e)))))

  (testing "const"
    (let [a (wire)
          b (wire)
          c (wire)
          add (adder a b c)
          a-const (const a 2)]
      (is (thrown? Exception (set-v! a 4 'user)))
      (is (= nil (get-v c)))
      (set-v! b 1 'user)
      (is (= 3 (get-v c)))
      (is (= nil (forget-v! a 'user)))
      (is (= 'ok (forget-v! b 'user)))
      (is (= nil (get-v c)))
      (is (= nil (get-v b)))
      (set-v! c 5 'user)
      (is (= 3 (get-v b)))))

  (testing "adder"
    (let [a (wire)
          b (wire)
          c (wire)
          add (adder a b c)]
      ;; (prn add)
      (inform-got-val add)
      (is (= nil (get-v c)))
      (inform-no-val add)
      (is (= nil (get-v c)))
      (set-v! a 1 'user)
      (is (= 1 (get-v a)))
      (is (= nil (get-v c)))
      (set-v! b 2 'user)
      (is (= 3 (get-v c)))
      (is (thrown? Exception (set-v! c 4 'user)))
      (is (not= 'ok (forget-v! c 'user)))
      (is (= 3 (get-v c)))
      (is (= 'ok (forget-v! a 'user)))
      (is (= nil (get-v c)))
      (set-v! c 4 'user)
      (is (= 2 (get-v a)))))

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

(comment
  (Math/ulp 2))
