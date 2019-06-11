(ns sicp.ch3.constraints
  (:require [clojure.test :refer :all]))

(defprotocol IWire
  (get-val [_])
  (has-val? [_])
  (set-val! [_ sender-box v]
    "if can, set new-val and send message to all connected boxes, except sender-box")
  (add-box! [_ b])
  (del-val! [_ sender-box]))

(defprotocol IBox
  ;; (can-set? [_ w])
  ;; (new-val [_] "box -> bool, true if new val is set"))
  (new-val [_] "box -> nil")
  (del-val [_ w] "box -> nil"))

;; (defn -can-set? [& ws]
;;   (= 1 (count (filter (complement has-val?) ws))))
;;
;; (defn -new-val [box can-set-fn set-fn]
;;   (if (can-set-fn)
;;     (set-fn)
;;     (throw (Exception. (str "Can't set new-val for box " box)))))


(def log prn)

(defrecord Wire [v boxes w-name]
  ;; Object
  IWire

  ;; (toString [_] (str "Wire v: " @v " w-name " w-name))
  (get-val [_] @v)
  (has-val? [_] (some? @v))
  (add-box! [_ b] (swap! boxes conj b))

  (set-val!  [w sender-box v-val]
    (reset! v v-val)
    (doseq [b (filter #(not= % sender-box) @boxes)]
      (new-val b)))

    ;; (let [;;v0 (get-val w)
    ;;       _ (reset! v v-val)
          ;; boxes (filter #(not= % sender-box) @boxes)
    ;;       ;; ok (every? identity (map new-val boxes))
    ;;       ]
    ;;   (loop [boxes @boxes processed '()]
    ;;     (cond
    ;;       (empty? boxes) true
    ;;       (= (first boxes) sender-box) (recur (rest boxes))
    ;;       :else (if (new-val (first boxes))
    ;;                          (recur (rest boxes))
    ;;                          (do
    ;;                            (reset! v v0)
    ;;                            (log "can't set " v-val "for box" (first boxes) "wire" w)
    ;;                            false
    ;;                            ))
    ;;
    ;;       ))
    ;;
    ;;   ;; (if (every? can-set? boxes)
    ;;   ;;   (do
    ;;   ;;     (reset! v v-val)
    ;;   ;;     (doseq [b boxes] (new-val b)))
    ;;   ;;   (throw (Exception. (str "Can't set val " v-val " for wire " w))))
    ;;   )
  (del-val! [w sender-box]
    (reset! v nil)
    (doseq [b (filter #(not= % sender-box) @boxes)]
      (del-val b w))))

(defn make-wire [w-name] (Wire. (atom nil) (atom []) w-name))

(defrecord Const [c w]
  IBox
  (new-val [_] (cond
                 (nil? (get-val w)) (set-val! w _ c)
                 (not= (get-val w) c) (throw (Exception. (str "new-val for Const " c "wire " w)))))

  (del-val [b _] (if (nil? (get-val w))
                   (set-val! w b c)
                   (throw (Exception. (str "del-val for Const " c "wire " w))))))

(defn make-const [c w]
  (let [b (Const. c w)]
    (add-box! w b)
    (set-val! w b c)
    b))

(defrecord Add [w-a w-b w-c]
  IBox
  (new-val [box]
    (let [a (get-val w-a)
          b (get-val w-b)
          c (get-val w-c)]
      (cond
        (and (every? some? [a b c]) (not= c (+ a b)))
        (throw (Exception. (format "Add can't set new-val for %d %d %d" a b c)))
        (and a b) (set-val! w-c box (+ a b))
        (and a c) (set-val! w-b box (- c a))
        (and b c) (set-val! w-a box (- c b))
        :else true)))

  (del-val [b w]
    (doseq [w (filter #(not= % w) [w-a w-b w-c])]
    ;; (doseq [w  [w-a w-b w-c]]
      (del-val! w b))
    ;; (new-val b)
    ))

(defn make-add [a b c]
  (let [box (Add. a b c)]
    (doseq [w [a b c]] (add-box! w box))))

(deftest test-boxes

  (testing "Adds and Consts"

    (let [a (make-wire 'a)
          b (make-wire 'b)
          c (make-wire 'c)
          add (make-add a b c)
          c-a (make-const 1 a)
          ;; c-b (make-const 2 b)
          d (make-wire 'd)
          e (make-wire 'e)
          add (make-add c d e)]
      (is (nil? (get-val e)))
      (set-val! d 'user 3)
      (is (nil? (get-val e)))
      (set-val! e 'user 6)
      (is (= 2 (get-val b)))

      ;; (is (= 6 (get-val e)))
      (del-val! e 'user)
      (del-val! d 'user)
      (is (nil? (get-val e)))
      (is (nil? (get-val b))))

    (let [a (make-wire 'a)
          b (make-wire 'b)
          c (make-wire 'c)
          add (make-add a b c)
          c-a (make-const 1 a)
          c-b (make-const 2 b)
          d (make-wire 'd)
          e (make-wire 'e)
          add (make-add c d e)
          d-a (make-const 3 d)]
      (is (= 6 (get-val e)))
      (del-val! e 'user)
      (is (= 6 (get-val e)))))

  (testing "Add and Const"
    (let [a (make-wire 'a)
          b (make-wire 'b)
          c (make-wire 'c)
          add (make-add a b c)
          c-a (make-const 1 a)]
      (is (= 1 (get-val a)))
      (is (nil? (get-val b)))
      (set-val! b 'user 2)
      (is (= 3 (get-val c)))
      (is (thrown? Exception (set-val! b 'user 4)))
      ;; (del-val! b 'user)
      ;; (del-val! c 'user)
      (del-val! a 'user)
      ;; (is (= 2 (get-val b)))
      ;; (is (= 3 (get-val c)))
      ;; (is (nil? (get-val c)))
      ;; (is (nil? (get-val c)))
      (is (every? nil? (map get-val [b c])))
      (is (= 1 (get-val a)))
      (set-val! c 'user 2)
      (is (= 1 (get-val b)))

      ;; (is (nil? (get-val c)))
      ;; (is (nil? (get-val b)))
      ;; (del-val! b 'user)
      (make-const 1 b)
      (del-val! c 'user)
      (is (= 2 (get-val c)))))

  (testing "Add"
    (let [a (make-wire 'a)
          b (make-wire 'b)
          c (make-wire 'c)
          add (make-add a b c)]
      (is (nil? (get-val a)))
      (set-val! a 'user 1)
      (is (= 1 (get-val a)))
      (is (nil? (get-val b)))
      (set-val! b 'user 2)
      (is (= 3 (get-val c)))
      (is (thrown? Exception (set-val! b 'user 4)))
      (del-val! c 'user)
      (is (every? nil? (map get-val [a b c])))))

  (testing "Const"
    (let [w (make-wire 'w)
          c (make-const 1 w)]
      (is (= 1 (get-val w)))
      (is (has-val? w))
      (del-val! w 'user)
      (is (= 1 (get-val w)))))

  (testing "2 Const"
    (let [w (make-wire 'w)
          c (make-const 1 w)
          c1 (make-const 1 w)]
      (is (= 1 (get-val w)))
      (is (thrown? Exception (make-const 2 w))))

    (let [w (make-wire 'w)
          c (make-const 1 w)]
      ;; (prn (:c c))
      (is (thrown? Exception (make-const 2 w)))
      ;; BUT new val is set
      (is (= 2 (get-val w)))
      (is (thrown? Exception (set-val! w 'foo 3)))
      ;; BUT new val is set
      (is (= 3 (get-val w)))
      ;; BUT it's still throw Exception
      (is (thrown? Exception (make-const 3 w)))
      (is (thrown? Exception (set-val! w 'foo 3)))
      (is (thrown? Exception (set-val! w 'foo nil)))
      ;; (prn (count @(:boxes w)))
      ))

;; (testing "Add"
;;     (let [a (make-wire 'a)
;;           b (make-wire 'b)
;;           c (make-wire 'c)
;;           adder (Add. a b c)
;;           c-b (make-const 1 b)
;;           ]
;;       (is (set-val! a 'user 2))
;;       (is (= 1 (get-val b)))
;;       (is (= 2 (get-val a)))
;;       (is (= 3 (get-val c)))
;;       ))
  )

(test-boxes)

(comment
  (every? identity [])
  (every? identity [1 ""])
  (every? identity [1 nil])
  (every? identity [1 false])
  (bit-xor 1 0 nil 1)
  (some? (atom nil)))
