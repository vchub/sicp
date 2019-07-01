(ns sicp.ch3.streams.pairs
  (:require [clojure.test :refer :all]
            [sicp.ch3.streams.stream-cons :refer [cons-m car cdr take-s stream-ref] :as ss]
            [sicp.ch3.streams.infinite :refer [close-enough partial-sums abs] :as inf]))

(def integers (cons-m 0 (ss/map-stream inc integers)))

(defn concat-s "Stream, Stream -> Stream"
  [xs ys]
  (cond
    (nil? xs) ys
    ;; (nil? ys) xs
    :else (cons-m (car xs) (concat-s (cdr xs) ys))))

(defn interleave-s "Stream, Stream -> Stream"
  ([s1 s2]
   (if (nil? s1)
     s2
     (cons-m (car s1) (interleave-s s2 (cdr s1)))))
  ([s1 s2 & more]
   (when (not (nil? s1))
     (let [s (apply vector s2 more)]
       (cons-m (car s1) (apply interleave-s (conj s (cdr s1))))))

   ;; (when (seq more)
   ;;   (let [s (interleave-s s1 s2)]
   ;;     (apply interleave-s (list* s more))))
   ))

(defn prime? [n] (.isProbablePrime (biginteger n) 5))

(defn pairs "Stream, Stream -> Stream of [s t]"
  [s t]
  (cons-m (vector (car s) (car t))
          (interleave-s
           (ss/map-stream #(vector (car s) %) (cdr t))
           (pairs (cdr s) (cdr t)))))

(def int-pairs (pairs integers integers))
(def int-pairs-for (for [x (range) y (range (inc x))] [y x]))

(defn take-while-s "predicat Stream -> vector"
  [pred s]
  (loop [s s acc []]
    (cond
      (nil? s) acc
      (pred (car s)) (recur (cdr s) (conj acc (car s)))
      :else acc)))

(defn all-pairs "Stream, Stream -> Stream of [s t]"
  [s t]
  (cons-m (vector (car s) (car t))
          (interleave-s
           (ss/map-stream #(vector (car s) %) (cdr t))
           (ss/map-stream #(vector % (car t)) (cdr s))
           (all-pairs (cdr s) (cdr t)))))

(defn tripples
  "Stream, Stream, Stream -> Stream of [ai bj ck]
  i<=j<=k"
  [a b c]
  (cons-m [(car a) (car b) (car c)]
          (interleave-s
           (ss/map-stream (fn [[b c]] [(car a) b c]) (pairs (cdr b) (cdr c)))
           (tripples (cdr a) (cdr b) (cdr c))))
  ;; (ss/map-stream (fn [[a [b c]]] [a b c]) (pairs a (pairs b c)))
  )

(defn square [x] (* x x))

(defn hypoth [a b] (+ (square a) (square b)))

(def naturals (cons-m 1 (ss/map-stream inc naturals)))

(defn pythagorean-tripple "i^2 + j^2 = k^2 i<=j<=k"
  []
  (ss/filter-stream (fn [[a b c]] (= (hypoth a b) (square c)))
                    (tripples naturals naturals naturals)))

(defn pythagorean-tripple-map
  "num -> (set [i j k])
  i^2 + j^2 = k^2 i<=j<=k<= n"
  [n]
  (reduce
   (fn [acc [i j]] (let [k (hypoth i j)
                         k-root (int (Math/sqrt k))]
                     (if (= k (square k-root))
                       (conj acc [i j k-root])
                       acc)))
   #{}
   (for [j (range 1 n)
         i (range 1 (inc j))]
     [i j])))

(deftest test-pairs

  (testing "ex 3.69"
    (let [ts (tripples integers integers integers)
          ts-100 (set (take-s ts 100))
          exp (set [[0 0 0] [1 1 1] [1 2 2] [2 2 2] [0 0 2]])
          errors (set [[1 0 0] [1 0 1] [1 2 1] [2 1 1] [2 0 2]])
          ]

      ;; (is (= [[0 0 0] [0 0 1] [1 0 1] [0 1 1]] (take-s ts 8)))
      (is  (some #(= % [1 3 4]) ts-100))
      (is  (some exp ts-100))
      (is  (not (some errors ts-100)))
      )

    (let [
          p-triple-map (pythagorean-tripple-map 100)
          primitive-triples (set '((3, 4, 5) (5, 12, 13) (8, 15, 17) (7, 24, 25)
                                             (20, 21, 29) (12, 35, 37) (9, 40, 41) (28, 45, 53)
                                             (11, 60, 61) (16, 63, 65) (33, 56, 65) (48, 55, 73)
                                             (13, 84, 85) (36, 77, 85) (39, 80, 89) (65, 72, 97)))
          pyth-s (set (take-s (pythagorean-tripple) 4))
          ]
      ;; (prn (clojure.set/difference primitive-triples p-triple-map))
      (is (clojure.set/subset? primitive-triples p-triple-map))
      (is (clojure.set/subset? (set [[3 4 5] [5 12 13]]) pyth-s))
      (is (not (clojure.set/subset? (set [[3 4 5] [5 12 13] [7 24 25]]) pyth-s)))
      ;; (prn "pythagorean-tripple-map 100" (time (do (pythagorean-tripple-map 100) nil)))
      ;; (prn "pythagorean-tripple 5" (time (take-s (pythagorean-tripple) 5)))
      ))

  (testing "ex 3.68"
    ;; (let [pairs-2 (fn pairs-2 [s t]
    ;;                 (interleave-s
    ;;                  (ss/map-stream #(vector (car s) %) t)
    ;;                  (pairs-2 (cdr s) (cdr t))))]
    ;;   (is (= [[0 0] [1 1] [0 1] [1 2]]
    ;;          (take-s (pairs-2 integers integers) 4)))
    ;;   (is  (some #(= % [3 4]) (take-s (pairs-2 integers integers) 100))))
    )

  (testing "ex 3.67"
    (let [all-pairs-for (for [x (range) y (range)] [x y])]
      (is (= [[0 0] [0 1] [0 2] [0 3]] (take 4 all-pairs-for)))
      (is (= [[0 0] [0 1] [1 0] [1 1] [0 2] [2 0] [1 2] [0 3] [3 0]]
             (take-s (all-pairs integers integers) 9)))))

  (testing "interleave-s"
    (let [i1 (reduce #(cons-m %2 %1) nil (range 10 -1 -1))]
      (is (= [0 0 0 1 1 1 2 2 2 3 3] (take-s (interleave-s i1 i1 i1) 11)))
      (is (= [0 0 0 1 1 1 2 2 2 3 3] (take-s (interleave-s integers integers integers) 11)))))

  (testing "interleave-s, prime?"

    (is (= [[0 0] [0 1] [1 1]] (take-s int-pairs 3)))
    (is (not= (take 8 int-pairs-for) (take-s int-pairs 8)))

    (let [ps1 (ss/filter-stream (fn [[a b]] (prime? (+ a b))) int-pairs)
          ps2 (filter (fn [[a b]] (prime? (+ a b))) int-pairs-for)
          n 4]
      (is (= (set [[0 2] [1 1] [0 3] [1 2]]) (set (take-s ps1 4))))
      (is (= (set [[0 2] [1 1] [0 3] [1 2]]) (set (take 4 ps2))))
      (is (= (set (take-s ps1 n)) (set (take n ps2)))))

    (is (= [10 11 0 1] (take-s (concat-s (cons-m 10 (cons-m 11 nil)) integers) 4)))
    (is (= [0 1 2] (take-s (concat-s integers integers) 3)))
    (is (= [0 0 1 1 2 2 3] (take-s (interleave-s integers integers) 7)))
    (is (= '(2 1 0) (take-s (reduce (fn [acc x] (cons-m x acc)) nil (range 3)) 3)))

    (is (= [0 0 0 1 1 1 2 2 2 3] (take 10 (interleave (range) (range) (range)))))
    ;; (is (= (take-s (interleave-s integers integers integers) 10)
    ;;        (take 10 (interleave (range) (range) (range)))))
    (is (prime? 7))
    (is (not (prime? 217))))

  (testing "ex 3.66"
    (is (= [0 1 2 3] (take-while-s #(< % 4) integers)))
    (is (= (dec 2) (count (take-while-s #(not= [0 1] %) int-pairs))))
    (is (= (dec 4) (count (take-while-s #(not= [0 2] %) int-pairs))))
    ;; (is (= (int (dec (Math/pow 2 3))) (count (take-while-s #(not= [0 4] %) int-pairs))))
    (is (= (int (dec (* 2 5))) (count (take-while-s #(not= [0 5] %) int-pairs))))

    ;; (is (= (int (dec (* 4 5))) (count (take-while-s #(not= [1 5] %) int-pairs))))
    ;; (is (= (int (dec (* 4 5))) (count (take-while-s #(not= [2 5] %) int-pairs))))

    (is (= 199 (count (take-while-s #(not= [0 100] %) int-pairs))))
    ;; (is (= 199 (count (take-while-s #(not= [1 100] %) int-pairs))))
    ;; (is (= 199 (count (take-while-s #(not= [2 100] %) int-pairs))))
    )

  (testing "integers, for and mapcat"
    (is (= [0 1 2 3] (take-s integers 4)))

    (let [ps-cat (mapcat (fn [x] (map #(vector % x) (range (inc x)))) (range))]
      (is (= [[0 0] [0 1] [1 1]] (take 3 int-pairs-for)))
      (is (= [[0 0] [0 1] [1 1]] (take 3 ps-cat)))
      (is (= (take 10 int-pairs-for) (take 10 ps-cat))))))

(test-pairs)

(comment
  ((set '(1 2)) [2])
  (some (set '(1 2)) (set [2]))
  (some (set '(1 2)) (set [0 3])))
