(ns sicp.ch2.poly.arith
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [clojure.math.combinatorics :as combo]))

;; dispatch table
(def f-tbl (atom {}))

(defn put [op a-types f]
  (swap! f-tbl assoc-in [op a-types] f)
  )


(defn atype[x]
(let [t (:t (meta x))]
    (cond
      (not (nil? t)) t
      (= (type x) long) 'long
      (number? x) 'real
      :else nil
      ))
  )

(defn apply-genric [op & args]
  (let [a-types (map atype args)
        proc (get-in @f-tbl [op a-types])]
    (if proc
      (apply proc args)
      (throw (Exception. (str "no method for these op args: " op " " (seq a-types)))))))

(defn install-complex []
  (letfn [(sq[x] (* x x))
          (real[z] (first z))
          (img[z] (second z))
          (make[r i] ^{:t 'complex} [r i])
          (magn[z] (Math/sqrt (+ (sq (real z)) (sq (img z)))))
          (angle[z] (Math/atan (/ (img z) (real z))))
          (add [x y] (make (+ (real x) (real y)) (+ (img x) (img y))))
          (mul[x y] (let [m (* (magn x) (magn y))
                          a (+ (angle x) (angle y))
                          r (* m (Math/cos a))
                          i (* m (Math/sin a))]
                      (make r i)))
          ;; (coerce[x] (when (number? x) ^{:t real}[x]))
          ]
    ;; (put 'coerce '(complex) coerce)
    (put 'raise '(complex) (fn[x] nil))
    (put 'make '(real real) make)
    (put 'real '(complex) real)
    (put 'img '(complex) img)
    (put 'magn '(complex) magn)
    (put 'angle '(complex) angle)
    (put 'mul '(complex complex) mul)
    (put 'add '(complex complex) add)
    'done
    ))

(install-complex)

;; (defn complex[r i] (let [r ^{:t 'real}[r]
;;                          i ^{:t 'real}[i]]
;;                      (apply-genric 'make r i)))
(defn complex[r i] (apply-genric 'make r i))
(defn real[z] (apply-genric 'real z))
(defn img[z] (apply-genric 'img z))
(defn magn[z] (apply-genric 'magn z))
(defn mul[x y] (apply-genric 'mul x y))
(defn add[x y] (apply-genric 'add x y))

(testing
  (let [z (complex 1 2)]
    (is (= 1 (real z)))
    (is (= 2 (img z)))
    (is (= 5.0 (magn (complex 3 4))))
    (is (= [4 6] (add z (complex 3 4))))
    (is (= 10.0 (img (mul z (complex 3 4)))))
    (is (= -5.0 (real (mul z (complex 3 4)))))
    )

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
    (is (= 'complex (:c (meta y))))
    )
  )

(defn cartesian
  ([xs ys]
  (let [prod (fn[x] (map #(vec [x %]) ys))]
    (mapcat prod xs)))

  ([xs ys & zs]
  (loop [xs (cartesian xs ys) zs zs]
    (if (empty? zs)
      xs
      (recur (map #(concat (first %) (rest %)) (cartesian xs (first zs))) (rest zs)))
    ))
  )

(testing
  (is (= [] (cartesian [] [1 2])))
  (is (= [[1 3] [1 4]] (cartesian [1] [3 4])))
  (is (= [[1 3] [1 4] [2 3] [2 4]] (cartesian [1 2] [3 4])))
  (is (= [[1 3 5] [1 4 5] [2 3 5] [2 4 5]] (cartesian [1 2] [3 4] [5])))
  (is (= [[1 2 [1]] [1 2 [2]]] (cartesian [1] [2] [[1] [2]])))
  (is (= (combo/cartesian-product [1 2] [3 4] [5 [6]]) (cartesian [1 2] [3 4] [5 [6]])))
  )
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
  (meta '())
  (meta #' +)
  (meta #' /)
  (flatten [[1] [2]])
  (flatten [[[1]] [2]])
  (mapcat identity [[[1]] [2]])
  )
