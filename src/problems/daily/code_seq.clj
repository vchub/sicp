(ns problems.daily.code-seq
  (:require [clojure.test :refer :all]))

;; task content:

(defn next-cont "(xxyx..)-> ((xx) (yx..))"
  [xs]
  (loop [acc (list (peek xs)) xs (pop xs)]
    ;; (prn acc xs)
    (cond
      (empty? xs) [acc xs]
      (not= (peek acc) (peek xs)) [acc xs]
      :else (recur (conj acc (peek xs)) (pop xs)))))

(defn nxt-elem
  [xs]
  (->> xs
       (partition-by identity)
       (map (fn [x] (list (count x) (first x))))
       flatten))

(defn encode
  [n]
  (reduce (fn [acc _] (nxt-elem acc)) [1] (range (dec n))))

(defn encode-seq
  ([] (encode-seq '(1)))
  ([x] (lazy-seq (cons x (encode-seq (nxt-elem x))))))

(testing
 (is (= [[1] [2]] (next-cont '(1 2))))
  (is (= [[1 1] [2]] (next-cont '(1 1 2))))
  (is (= [[1 1] [2 2 3]] (next-cont '(1 1 2 2 3))))

  (is (= [1] (encode 1)))
  (is (= [1 1] (encode 2)))
  (is (= [2 1] (encode 3)))
  (is (= [1 2 1 1] (encode 4)))
  (is (= [1 1 1 2 2 1] (encode 5)))

  (let [codes (encode-seq)]
    (is (= [1] (nth (take 1 codes) 0)))
    ;; (is (= 1 (take 4 codes) ))
    (is (= [1 1] (nth (take 2 codes) 1)))
    (is (= [1 1 1 2 2 1] (nth (take 5 codes) 4)))))

(comment
  (partition-by identity [1 1 2 3 3]))
