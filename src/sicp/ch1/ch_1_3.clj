(ns sicp.ch1.ch_1_3
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [sicp.ch1.misc :as msc]))

(defn sum-rec
  "(num->num) num (num->num) num"
  [term a nxt b]
  (if (> a b)
    0
    (+ (term a) (sum-rec term (nxt a) nxt b))))

(testing
 (let [add2 (fn [a] (+ a 2))]
   (is (= 1 (sum-rec identity 0 inc 1)))
   (is (= 6 (sum-rec identity 0 inc 3)))
   (is (= 55 (sum-rec identity 0 inc 10)))
   (is (= (reduce + (range 11)) (sum-rec identity 0 inc 10)))
   (is (= 6 (sum-rec identity 0 add2 5)))
   (is (= 14 (sum-rec add2 0 inc 3)))
   (is (= 3025 (sum-rec #(* % % %) 0 inc 10)))))

(defn integral
  [f a b dx]
  (let [nxt-x (fn [x] (+ x dx))]
    (* (sum-rec f (+ a (/ dx 2)) nxt-x b) dx)))

(defn sqr [x] (* x x))
(defn cube [x] (* x x x))

(testing
 (let [t [{:got (integral sqr 0 1 0.1) :exp 0.333}
          {:got (integral cube 0 1 0.1) :exp 0.25}]
       eps 0.01]
   (doseq [{:keys [got exp]} t]
      ;; (prn got exp)
     (is (and (< got (+ exp eps)) (> got (- exp eps)))))))

(defn lazy-4-2
  [n]
  (if (zero? n)
    nil
    (lazy-seq (cons (if (even? n) 4 2) (lazy-4-2 (dec n))))))

(defn simpson-seq
  [f a n h]
  (letfn [(gen [n x]
            (if (zero? n)
              nil
              (lazy-seq (cons (if (even? n)
                                (* 4 (f x))
                                (* 2 (f x))) (gen (dec n) (+ a h))))))]
    (gen n a)))

(defn simpson-integral
  [f a b n]
  (reduce + (simpson-seq f a n (/ (+ a b) n))))

(defn simpson
  [f a b n]
  (let [h (/ (- b a) n)
        term (fn [k]
               (cond
                 (zero? k) (f a)
                 (= n k) (f b)
                 (even? k) (* 2 (f (+ a (* k h))))
                 :else (* 4 (f (+ a (* k h))))))]
    (* (/ h 3) (reduce + (map term (range (inc n)))))))

(testing
 (is (= [4 2] (take 2 (lazy-4-2 2))))
  (is (= [4 2 4 2] (lazy-4-2 4)))
  (is (= [0 0.2] (simpson-seq identity 0 2  0.1)))
  (is (= 0.2 (simpson-integral identity 0 0.2 2)))
  (is (= 1/4 (simpson cube 0 1 100)))
  (is (= 1/3 (simpson sqr 0 1 100))))

(defn sum-iter
  [term a nxt b]
  (letfn [(iter [a result]
            (if (> a b)
              result
              (iter (nxt a) (+ result (term a)))))]
    (iter a 0)))

(testing
 (is (= 55 (sum-iter identity 0 inc 10))))

(defn reduce-iter
  [term a nxt b acc]
  (loop [a a acc acc]
    (cond
      (>= a b) acc
      :else
      (recur (nxt a) (term acc a)))))

(defn product-a-b
  [a b]
  (reduce-iter * a inc b 1))

(defn pi-aprox
  [n]
  (let [f (fn [[acc c d] i] (if (even? i)
                              [(* acc (/ c d)) (+ 2 c) d]
                              [(* acc (/ c d)) c (+ 2 d)]))]

    (-> (reduce f [1 2 3] (range n))
        (get 0)
        (float)
        (* 4))))

(defn in-d-range
  [a x dx]
  (and (< a (+ x dx))
       (> a (- x dx))))

(testing
 (is (= 1 (product-a-b 1 1)))
  (is (= 1 (product-a-b 1 2)))
  (is (= 2 (product-a-b 1 3)))
  (is (in-d-range (pi-aprox 20) Math/PI 0.1)))

(defn prime-sqr
  [a b]
  (let [a (cond
            (= 2 a) 2
            (even? a) (inc a)
            :else a)]
    (->> (range a b)
         (filter msc/prime?)
         (map sqr)
         (reduce +))))

(defn gcd [a b]
  (let [r (mod a b)]
    (cond
      (zero? r) b
      :else (recur b r))))

(testing
 (is (= 13 (prime-sqr 2 4)))
  (is (= 38 (prime-sqr 2 6)))
  (is (= 1 (gcd 3 2)))
  (is (= 2 (gcd 6 4))))

(defn abs [x] (if (pos? x) x (* -1 x)))
(defn close-enough [a b dx] (< (abs (- b a)) dx))

(defn bi-root
  "half interval method of finding root of f: f(x)=0 on [a b]"
  [f neg pos dx]
  (let [average (fn [a b] (/ (+ a b) 2))
        mid (float (average neg pos))
        y (f mid)]
    ;; (prn mid y neg pos (close-enough neg pos dx))
    (cond
      (close-enough neg pos dx) mid
      (< y 0) (recur f mid pos dx)
      (> y 0) (recur f neg mid dx)
      :else mid)))

(defn bi-root-method
  "half interval method of finding root of f: f(x)=0 on [a b]"
  [f a b dx]
  (cond
    (and (neg? (f a)) (pos? (f b))) (bi-root f a b dx)
    (and (neg? (f b)) (pos? (f a))) (bi-root f b a dx)
    :else (throw (Exception. (format "Values are not of opposite sign %d %d" a b)))))

(testing
 (is (= 0. (bi-root (fn [x] x) -1 1 0.1)))
  (is (= -0.5 (bi-root (fn [x] (+ (* 2 x) 1)) -1 1 0.1)))
  (is (= -0.5 (bi-root-method (fn [x] (+ (* 2 x) 1)) 1 -1 0.1)))
  (is (= (close-enough 3.14 (bi-root-method (fn [x] (Math/sin x)) 2 4 0.1) 0.1))))

(defn fixed-point
  "(num)-> num, num -> num"
  [f guess dx]
  (loop [fx (f guess) guess guess]
    (if (close-enough fx guess dx)
      guess
      (recur (f fx) fx))))

(defn golden-ratio
  [dx]
  (float (fixed-point (fn [x] (+ 1 (/ 1 x))) 1 dx)))

(defn cont-frac
  "(num)-> num, (num)->num, num -> num"
  [n d k]
  (loop [k k acc 0]
    ;; (prn k acc)
    (cond
      (neg? k) acc
      :else
      (recur (dec k) (/ (n k) (+ (d k) acc))))))

(defn reciprical [x] (/ 1 x))

(defn golden-ratio-frac
  [k]
  (let [n (fn [k] 1)]
    (reciprical (cont-frac n n k))))

(defn tan-frac
  [x k]
  (let [n (fn [k] (if (zero? k) x (* -1 (* x x))))
        d (fn [k] (inc (* 2 k)))]
    (float (cont-frac n d k))))

(testing
 (is (close-enough 0.74 (fixed-point #(Math/cos %) 1 0.01) 0.01))
  (is (close-enough (Math/sqrt 2) (float (fixed-point #(/ (+ % (/ 2 %)) 2) 1 0.01)) 0.01))
  (is (close-enough (Math/sqrt 3) (float (fixed-point #(/ (+ % (/ 3 %)) 2) 1 0.01)) 0.01))
  (is (close-enough  1.61 (golden-ratio 0.001) 0.01))
  (is (close-enough  (/ (+ 1 (Math/sqrt 5)) 2) (golden-ratio 0.001) 0.001))

  (is (close-enough  (/ (+ 1 (Math/sqrt 5)) 2) (golden-ratio-frac 10) 0.0001))

  (is (close-enough  (Math/tan 2) (tan-frac 2 10) 0.0001)))

(defn average [a b] (/ (+ a b) 2))

(defn average-damp
  [f]
  (fn [x] (average x (f x))))

(defn square [x] (* x x))

(defn sqr-damp
  [x dx]
  (float (fixed-point
          (average-damp (fn [y] (/ x y))) 1 dx)))

(defn cube-root-damp
  [x dx]
  (float (fixed-point
          (average-damp (fn [y] (/ x (square y)))) 1 dx)))

(testing
 (is (close-enough (Math/sqrt 2) (sqr-damp 2 0.01) 0.01))
  (is (close-enough (Math/sqrt 3) (sqr-damp 3 0.01) 0.01))
  (is (close-enough (Math/pow 3 (/ 1 3)) (cube-root-damp 3 0.01) 0.01)))

(defn deriv [f dx] (fn [x] (/ (- (f (+ x dx)) (f x)) dx)))

(defn newton-transform
  [g dx]
  (let [dg (deriv g dx)]
    (fn [x] (- x (/ (g x) (dg x))))))

(defn newton-method
  [g guess dx]
  (fixed-point (newton-transform g dx) guess dx))

(defn sqr-newton
  [x dx]
  (float (newton-method (fn [y] (- (square y) x)) 1 dx)))

(defn fixed-point-of-transform
  [g transform guess dx]
  (fixed-point (transform g) guess dx))

(defn sqr-damp-transf
  [x dx]
  (fixed-point-of-transform (fn [y] (/ x y)) average-damp 1 dx))

(defn sqr-newton-trans
  [x dx]
  (fixed-point-of-transform #(- (square %) x) #(newton-transform % dx) 1 dx))

(testing
 (is (close-enough (Math/sqrt 2) (sqr-newton 2 0.01) 0.01))
  (is (close-enough (Math/sqrt 5) (sqr-newton 5 0.01) 0.01))

  (is (close-enough (Math/sqrt 2) (sqr-damp-transf 2 0.01) 0.01))
  (is (close-enough (Math/sqrt 2) (sqr-newton-trans 2 0.01) 0.01)))

(comment
  (Math/sin 2)
  (Math/sin 4)
  (let [f (fn [x] x)]
    (def y 3)
    (def z (fn [] y))
    (+ (f 1) y (z)))

  (let [f (fn [g] (g 2))]
    (f sqr)
    (f (fn [x] (* x x x)))
    ;; (f f)
    )

  (msc/prime? 4)
  (integral cube 0 1 0.1)
  (map-indexed vector (range 0 1 0.2))
  (float 2/3)
  (get [0 3 2] 1))
