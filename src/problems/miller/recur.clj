(ns problems.miller.recur
  (:require [clojure.test :refer :all]))

(def M)
(def m)

(defn F [n] (if (zero? n)
              1
              (- n (M (F (- n 1))))))

(defn M [n] (if (zero? n)
              0
              (- n (F (M (- n 1))))))

(def f (memoize (fn [n] (if (zero? n)
                          1
                          (- n (m (f (- n 1))))))))

(def m (memoize (fn [n] (if (zero? n)
                          0
                          (- n (f (m (- n 1))))))))

;; ====================
(def f-seq (map f (iterate inc 0)))
(def m-seq (map m (iterate inc 0)))

;; (def f-seq-gen)
;; (def m-seq-gen)

;; (defn f-seq-gen
;;   ([] (f-seq 0))
;;   ([n] (if (< n 2)
;;          (lazy-cat [1 1] (f-seq-gen 2))
;;          (lazy-seq (cons (- n (get m-seq (get f-seq (dec n)))) (f-seq-gen (inc n)))))))
;;
;; (defn m-seq-gen
;;   ([] (m-seq 0))
;;   ([n] (if (< n 2)
;;          (lazy-cat [0 0] (m-seq-gen 2))
;;          (lazy-seq (cons (- n (get f-seq (get m-seq (dec n)))) (m-seq-gen (inc n)))))))

;; (defn f-seq
;;   ([] (f-seq 0))
;;   ([n] (lazy-seq (cons (if (zero? n)
;;                          1
;;                          (- n (get (m-seq) (get (f-seq) (dec n)) 0))) (f-seq (inc n))))))

;; (def f-seq (f-seq-gen))
;; (def m-seq (m-seq-gen))

;; (defn m-seq
;;   ([] (m-seq 0))
;;   ([n] (lazy-seq (cons (if (zero? n)
;;                        0
;;                        (- n (get (f-seq) (get (m-seq) (dec n)) 1))) (m-seq (inc n))))))


(deftest test-rec0
  (is (= [1, 1, 2, 2, 3, 3, 4, 5, 5, 6, 6, 7, 8] (map F (range 13))))
  (is (= [0, 0, 1, 2, 2, 3, 4, 4, 5, 6, 6, 7, 7, 8, 9] (map M (range 15))))
  (is (= [1, 1, 2, 2, 3, 3, 4, 5, 5, 6, 6, 7, 8] (map f (range 13))))
  (is (= [0, 0, 1, 2, 2, 3, 4, 4, 5, 6, 6, 7, 7, 8, 9] (map m (range 15))))
  ;; (let [n 100]
  ;;   (prn "F n = " n (time (do (F n))))
  ;;   (prn "f n = " n (time (do (f n))))
  ;;   )

  (is (= 124 (f 200)))
  (is (= 155 (f 250)))

  ;; (let [n 250
  ;;       _ (prn "f " n)
  ;;       got (time (f n))]
  ;;   (is (= 155 got)))

  ;; (is (thrown? Exception (f 10000)))

    (is (= [1] (take 1 f-seq)))
    (is (= [1 1] (take 2 f-seq)))
    (is (= [1 1 2] (take 3 f-seq)))
    (is (= [1, 1, 2, 2, 3, 3, 4, 5, 5, 6, 6, 7, 8] (take 13 f-seq)))
  (is (= [0, 0, 1, 2, 2, 3, 4, 4, 5, 6, 6, 7, 7, 8, 9] (take 15 m-seq)))
  (is (= 155 (last (take 251 f-seq))))
  (is (= 6180 (last (take 10000 f-seq))))

  )

(test-rec0)
