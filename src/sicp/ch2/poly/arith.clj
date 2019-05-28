(ns sicp.ch2.poly.arith
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [clojure.math.combinatorics :as combo]))

;; dispatch table
(def f-tbl (atom {}))

(defn put [op a-types f]
  (swap! f-tbl assoc-in [op a-types] f))

(defn atype [x]
  (let [t (or
            (:t (meta x))
            (:t x))]
    (cond
      (not (nil? t)) t
      ;; (= (type x) long) 'long
      (number? x) 'real
      (symbol? x) 'symbol
      :else nil)))

;; defined later
(def raise)
(def cartesian)

(defn log [x]
  (do
    (prn x)
    x))

(defn ancesstors
  "elem -> [symbol]"
  [x]
  (if (nil? x)
    nil
    (cons x (ancesstors (raise x)))))

(defn ancesstor-op [op args]
  (loop [args-s (->> (map ancesstors args)
                     (apply cartesian))]
    (if (empty? args-s)
      (throw (Exception. (str "no method for these op args: " op " " (seq args-s))))
      (let [a-types (map atype (first args-s))
            proc (get-in @f-tbl [op a-types])]
        (if (not (nil? proc))
          (apply proc (first args-s))
          (recur (rest args-s)))))))

(defn apply-genric [op & args]
  (let [a-types (map atype args)
        proc (get-in @f-tbl [op a-types])]
    (if proc
      (apply proc args)
      (ancesstor-op op args))))

(defn install-complex []
  (letfn [(sq [x] (* x x))
          (real [z] (first z))
          (img [z] (second z))
          (make [r i] ^{:t 'complex} [r i])
          (project [x] (real x))
          ;; (try-drop [z] (if (= z (raise (project z))) (project z) z))
          (try-drop [z] (if (zero? (img z)) (real z) z))
          (magn [z] (Math/sqrt (+ (sq (real z)) (sq (img z)))))
          (angle [z] (Math/atan (/ (img z) (real z))))
          (add [x y] (try-drop (make (+ (real x) (real y)) (+ (img x) (img y)))))
          (negate [z] (apply make (map #(* -1 %) z)))
          (sub [x y] (add x (negate y)))
          (make-mag-angle [m a] (make (* m (Math/cos a)) (* m (Math/sin a))))
          (mul [x y] (try-drop (let [m (* (magn x) (magn y))
                                     a (+ (angle x) (angle y))]
                                 (make-mag-angle m a))))
          (recip [z] (make-mag-angle (/ 1 (magn z)) (* -1 (angle z))))
          (div [x y] (mul x (recip y)))
          ;; (coerce[x] (when (number? x) ^{:t real}[x]))
          ]
    ;; (put 'coerce '(complex) coerce)
    (put 'raise '(complex) (fn [x] nil))
    (put 'make '(real real) make)
    (put 'real '(complex) real)
    (put 'img '(complex) img)
    (put 'magn '(complex) magn)
    (put 'angle '(complex) angle)
    (put 'mul '(complex complex) mul)
    (put 'add '(complex complex) add)
    (put 'sub '(complex complex) sub)
    (put 'div '(complex complex) div)
    (put 'project '(complex) project)
    (put 'try-drop '(complex) try-drop)
    'done))

(install-complex)

(defn complex [r i] (apply-genric 'make r i))
(defn raise [x] (apply-genric 'raise x))
;; (defn project [x] (apply-genric 'project x))
;; (defn try-drop [x] (apply-genric 'try-drop x))

(defn real [z] (apply-genric 'real z))
(defn img [z] (apply-genric 'img z))
(defn magn [z] (apply-genric 'magn z))
(defn angle [z] (apply-genric 'angle z))
(defn mul [x y] (apply-genric 'mul x y))
(defn div [x y] (apply-genric 'div x y))
(defn add [x y] (apply-genric 'add x y))
(defn sub [x y] (apply-genric 'sub x y))

(defn install-real []
  (put 'raise '(real) (fn [x] (complex x 0)))
  'done)

(install-real)

(defn install-small []
  ;; (put 'raise '(small) (fn [x] (complex (first x) 0)))
  (put 'raise '(small) (fn [x] (first x)))
  (put 'small '(real) (fn [x] ^{:t 'small} [x]))
  'done)

(install-small)

(defn small [z] (apply-genric 'small z))

(defn cartesian
  ([xs] (map vector xs))
  ([xs ys]
   (let [prod (fn [x] (map #(vec [x %]) ys))]
     (mapcat prod xs)))

  ([xs ys & zs]
   (loop [xs (cartesian xs ys) zs zs]
     (if (empty? zs)
       xs
       (recur (map #(concat (first %) (rest %)) (cartesian xs (first zs))) (rest zs))))))

(testing
  (is (= (complex 1 1) (complex 1 1)))
 (let [z (complex 1 2)]
   (is (= '((1 2)) (ancesstors z)))
   (is (= '(1 (1 0)) (ancesstors 1)))
   ;; (is (= '((real complex) (complex)) (map ancesstors [1 z])))
   ;; (is (= '((real complex) (complex complex)) (->> (map ancesstors [1 z])
   ;;                                         (apply cartesian))))

   (is (= 1 (real 1)))
   (is (= 0 (img 1)))

   (is (= 0 (sub z z)))
   ;; (is (= 0 (div 1 z)))
   ;; (is (= 0 (div (complex 1 1) z)))
   (is (= 8.0 (mul 4 2)))
   (is (= 2.0 (div 4 2)))
   ;; (is (= 2.0 (div 4.0 2/1)))

   (is (= [2 2] (add z 1)))
   (is (= [2 2] (add 1 z)))
   (is (= [2 2] (add (small 1) z)))
   (is (= 3 (add (small 1) (small 2))))
   (is (= 3 (add (small 1) (small 2))))
   (is (= 3 (add 1 (small 2))))
   ;; (is (= [2.0 4.0] (mul 2 z)))
   (is (= 0.0 (angle 1)))))

(testing
 (let [z (complex 1 2)]
   (is (= 1 (real z)))
   (is (= 2 (img z)))
   (is (= 5.0 (magn (complex 3 4))))
   (is (= [4 6] (add z (complex 3 4))))
   (is (= 10.0 (img (mul z (complex 3 4)))))
   (is (= -5.0 (real (mul z (complex 3 4))))))

  (is (= nil (:t {})))
  (is (= nil (meta 1)))
  (is (= nil (:t (meta 1))))
  ;; (prn f-tbl)
  ;; (is (empty? (:val f-tbl)))
  ;; (is (empty? @f-tbl))

  (let [x ^:hi [1 2]
        y ^{:c 'complex} [1 0]]
    (is (= [1 2] x))
    (is (= {:hi true} (meta x)))
    (is (:hi (meta x)))
    (is (= 'complex (:c (meta y))))))

(testing
 (is (= [] (cartesian [] [1 2])))
  (is (= [[1 3] [1 4]] (cartesian [1] [3 4])))
  (is (= [[1 3] [1 4] [2 3] [2 4]] (cartesian [1 2] [3 4])))
  (is (= [[1 3 5] [1 4 5] [2 3 5] [2 4 5]] (cartesian [1 2] [3 4] [5])))
  (is (= [[1 2 [1]] [1 2 [2]]] (cartesian [1] [2] [[1] [2]])))
  (is (= (combo/cartesian-product [1 2] [3 4] [5 [6]]) (cartesian [1 2] [3 4] [5 [6]]))))
;; (swap! f-tbl assoc :t 0)

;; (defn ff ^{:x 'int} [x] (inc x))

;; (testing
;;   ;; (is (= 'int (:x (meta ff))))
;;   (is (= 0 (:t @f-tbl)))
;;   (do
;;     (put '+ '(n n) +)
;;     (is (= 3 ((get-in @f-tbl '(+ (n n))) 1 2)))
;;     )
;;   )

(comment
  (log 3)
  (->> (map ancesstors [1 2]))

  (= (type 1) Long)
  (meta '())
  (meta #'+)
  (meta #'/)
  (flatten [[1] [2]])
  (flatten [[[1]] [2]])
  (mapcat identity [[[1]] [2]]))
