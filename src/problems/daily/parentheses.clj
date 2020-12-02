(ns problems.daily.parentheses
  (:require [clojure.string :as s]
            [clojure.test :refer :all]))
;; check if parentheses are balanced

;; lookup table
;; (def ltbl (into {} (for [[k v] (partition 2 "()[]{}<>")] {k v})))

(def ltbl (reduce conj (->> (partition 2 "()[]{}<>")
                            (map (fn [[k v]] {k v})))))

(defn in-keys [x] (some #{x} (keys ltbl)))
(defn in-vals [x] (some #{x} (vals ltbl)))

(defn process "[char] -> [char]"
  [s]
  (do
    (cond
      (empty? s) s
      (in-keys (first s)) (let [[c & tail] (process (rest s))
                                c0 (first s)]
                            (if (= c (get ltbl c0))
                              (process tail)
                              [(first s)]))
      (in-vals (first s)) s
      :else (process (rest s)))))

(defn balanced? "str -> bool"
  [s]
  (empty? (process (seq s))))

(defn bal2? "balanced? brackets?" [s]
  (let [tb  (reduce conj (->> (partition 2 "(){}[]")
                              (map (fn [[v k]] {k v}))))
        close? (fn [x] (contains? tb x))
        open? (fn [x] (some #{x} (vals tb)))]
    (loop [s s st []]
      (let [[c & s] s
            top (peek st)]
        (cond
          (nil? c) (empty? st)
          (open? c) (recur s (conj st c))
          (close? c)
          (cond
            (nil? top) false
            (= top (get tb c)) (recur s (pop st))
            :else false)
          :else (recur s st))))))

(first [])
(testing
 (is (bal2? "()"))
  (is (bal2? "([])"))
  (is (bal2? "(ab[c]d)"))
  (is (not (bal2? "(ab[c(]d)"))))

(comment
  (p ltbl)
  (into {} (for [p (s/split "ab cd" #" ")]
             (s/split p #"")))
  (get ltbl "{")
  (get ltbl \{)
  (get ltbl ".")
  (seq "ab()")
  (count (partition 2 "()[]"))
  (first "ab")
  (into {} (vector (seq "ab")))
  (into {} [[\a \b]])
  (into {} [[\a \b]])
  {\( \)}
  (seq (char-array "ab"))
  (seq "ab")
  (hash-map [(partition 2 "abcd")])
  (hash-map [1 2])
  (def ltbl (into {} (for [p (s/split "() [] <> {}" #" ")]
                       (s/split p #""))))
  (some (set [\a]) (keys ltbl))
  (rest (seq "abc"))
  (let [[x, y] [1]] (prn x y))
  (some #{3} [1,2])
  (some #{\c} ltbl)
  (some #{\(} ltbl))





;; (defn part2 [s] (partition 2 s))
;; (comment (part2 "abcd")
;;          (reverse "abc")
;;          (let [f (comp str +  )]
;;            (prn f)
;;            (prn (f 2 3))))
;;
;; (def xform (comp (map (fn[[k v]] {k v})) part2  ))
;; ;; (def ltbl (into {}  xform  "()[]{}<>" ))
