(ns sicp.ch4.debug-tutor
  (:require [clojure.test :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]))

(def property
  (prop/for-all [v (gen/vector gen/small-integer)]
                (let [s (sort v)]
                  (and (= (count v) (count s))
                       (or (empty? s)
                           (apply <= s))))))

;; (tc/quick-check 100 property)

;; ((defspec sort-property 100 property))
;; (sort-property)

(deftest run-quick-check
  (prn (gen/sample gen/small-integer))
  (prn (gen/sample gen/small-integer 20))
  (prn (gen/sample (gen/fmap set (gen/vector gen/small-integer))))
  (prn (->> (gen/vector gen/small-integer)
            (gen/fmap set)
            ;; (gen/such-that (complement empty?))
            (gen/such-that not-empty)
            (gen/sample)))
  (def nested-vector-of-boolean (gen/recursive-gen gen/vector gen/boolean))
  (prn (gen/sample nested-vector-of-boolean))
  ((defspec sort-property 100 property)))
(run-quick-check)

(defn foo
  [n]
  (cond (> n 40) (+ n 20)
        (> n 20) (- (first n) 20)
        :else 0))

(defn steps "[x] -> [x1 [x2 [x3 ...]]]"
  [xs]
  (if (empty? (drop 1 xs))
    [(first xs)]
    (vector (first xs) (steps (drop 1 xs)))))

(defn steps-lazy "[x] -> stream [x1 [x2 [x3 ...]]]"
  [xs]
  (if (empty? (drop 1 xs))
    [(first xs)]
    (lazy-seq (vector (first xs) (steps (drop 1 xs))))))

(defn rand-range [n] (repeatedly #(rand-int n)))

(defn quicksort-recur
  [xs]
  (cond
    (empty? xs) '()
    (empty? (rest xs)) [(first xs)]
    :else (let [d (/ (count xs) 2)
                ;; depends on distribution. may be not first
                p (first xs)
                x1 (quicksort-recur (filter #(<= % p) (rest xs)))
                x2 (quicksort-recur (filter #(> % p) (rest xs)))]
            (lazy-cat x1 [p] x2))))

(defn sort-parts [work]
  (lazy-seq
   (loop [[part & parts] work]
     (if-let [[pivot & work] (seq part)]
       (let [less? #(< % pivot)]
         (recur (list*
                 (filter less? work)
                 pivot
                 (remove less? work)
                 parts)))
       (when-let [[x & parts] parts]
         (cons x (sort-parts parts)))))))

(defn quicksort-lazy [xs]
  (sort-parts (list xs)))

(deftest test-debug-tutor
  (testing "quicksort"
    ;; (is (= 1 (take 15 (rand-range 10))))
    (is (= [1] (quicksort-recur [1])))
    (is (= [1 2] (quicksort-recur [2 1])))
    (is (= [1 2 3] (quicksort-recur [2 3 1])))
    (is (= [1 2 2 3] (quicksort-recur [2 3 2 1])))

    (is (= [1] (quicksort-lazy [1])))
    (is (= [1] (take 1 (quicksort-lazy [1]))))
    (is (= [1 2 2 3] (quicksort-lazy [2 3 2 1])))
    (is (= clojure.lang.LazySeq (type (quicksort-lazy [2 3 2 1]))))

    (let [xs (take 10 (rand-range 100))
          s (rand-range 10)]
      (is (= clojure.lang.LazySeq (type xs)))
      (is (= (sort xs) (quicksort-recur xs)))
      (is (= (sort xs) (quicksort-lazy xs)))
      ;; (is (= (sort (take 10 s)) (take 10 (quicksort-recur s))))
      ))

  (testing "steps"
    (is (= [1 [2]] (steps [1 2])))
    (is (= [1 [2 [3]]] (steps [1 2 3])))
    (is (= [1 [2]] (steps-lazy [1 2])))
    (is (= [1 [2 [3]]] (steps-lazy [1 2 3])))
    ;; (is (= 1 (first (steps-lazy (range 1e3)))))
    ;; (is (= 1 (first (drop 1 (steps-lazy (range 1e3))))))
    ))

(test-debug-tutor)

(comment
;;  to throw away all the potentially useful information: the exception classes, the stacktraces, and the error messages. To do this I will start a subrepl that snidely elides all information about exceptions caught by the REPL:
  (require '[clojure.main :as main])
  (main/repl :caught (fn [_] (println "Broken! HaHa!")))

  (def n 24)
  (foo 24)
  (list* [1 2] 3 [4 5] [6])
  (remove #(< % 3) [1 2 3 1 4 2])
  (if-let [x (seq [1])] x)
  (if-let [x (seq [])] 5)
  (if-let [x (seq 1)] x)
  (< 1 2 4 3)
  (apply < '(1 2 3)))
