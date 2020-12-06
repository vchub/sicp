(ns sicp.ch4.eval-data-table
  (:require [clojure.test :refer :all]))

(def eval-f)
(def apply-f)
(def table (atom {}))

(def primitive-proc-symbols #{'+ '- '* '/ '< '> '=})

(defn make-env [] (atom (-> (into {} (map #(vector % (eval %))) primitive-proc-symbols)
                            ;; (assoc 'false false)
                            )))

(defn install! [table fn-name tag proc] (swap! table assoc (list fn-name tag) proc))

(defn dispatcher [table fn-name]
  (letfn [(d [tag args]
            (let [f (get @table (list fn-name tag) (get @table (list 'eval-f :default)))]
              (apply f args)))]
    (fn [& args]
      (let [tag (first args)]
        (cond
          (or (number? tag) (string? tag) (boolean? tag)) (d 'self-eval args)
          (symbol? tag) (d 'symbol args)
          :else (d (first tag) args)
          ;; (seq? tag) (d (first tag) args)
          ;; :else (throw (Exception. (str "Unknown tag" tag "args" args)))
          )))))

(defn eval-f [exp env] ((dispatcher table 'eval-f) exp env))

(install! table 'eval-f 'self-eval (fn [exp env] exp))
(install! table 'eval-f 'symbol (fn [exp env] (get @env exp)))
(install! table 'eval-f 'def-f!
          (fn [exp env] (swap! env  assoc (second exp) (eval-f (nth exp 2) env))))
(install! table 'eval-f 'fn (fn [exp env] exp))
(install! table 'eval-f 'if (fn [exp env] (if (eval-f (second exp) env)
                                            (eval-f (nth exp 2) env)
                                            (eval-f (nth exp 3) env))))

(install! table 'eval-f :default (fn [exp env]
                                  ;; (prn "-------------------exp:" exp env)
                                   (apply-f
                                       ;; fn
                                    (eval-f (first exp) env)
                                       ;; args
                                    (map #(eval-f % env) (next exp))
                                    env)))

(defn proc-params [exp] (second exp))
(defn proc-body [exp] (drop 2 exp))
(defn extend-env [env params args]
  (let [p-env (into @env (map vector params args))]
    (atom p-env)))

(defn apply-f [proc args env]
  (if (seq? proc)
    (let [body (proc-body proc)
          env (extend-env env (proc-params proc) args)
          ret (reduce (fn [acc exp] (eval-f exp env)) nil body)]
      ret)
    (apply proc args)))

(deftest test-multi-table
  (testing "eval-f"
    (let [env (make-env)
          eval-f (fn [exp] (eval-f exp env))]

      (is (= 1 (eval-f 1)))

      (is (= 1 (eval-f 1)))
      (is (= "foo" (eval-f "foo")))
      (is (= nil (eval-f 'x)))
      (eval-f '(def-f! x 1))
      ;; ;; (prn env)
      (is (= 1 (eval-f 'x)))
      (is (= '(fn [x y] (+ x y)) (eval-f '(fn [x y] (+ x y)))))
      (is (= + (eval-f '+)))
      (is (= 5 (eval-f '(+ 2 3))))
      (is (= 2/3 (eval-f '(/ 2 3))))

      (is (= true (eval-f 'true)))
      (is (= 2/3 (eval-f '(if true (/ 2 3) (* 1 4)))))
      (is (= 2/3 (eval-f '(if "some" (/ 2 3) (* 1 4)))))
      (is (= 4 (eval-f '(if (< 1 0) (/ 2 3) (* 1 4)))))

      (eval-f '(def-f! sum (fn [x y] (+ x y))))
      (is (= 5 (eval-f '(sum 2 3))))
      (is (= 10 (eval-f '(sum (sum 2 3) (sum 2 3)))))
      (is (= 7 (eval-f '(sum (sum 2 3) (* 1 2)))))
      ;;
      ;; anonymous fn invocation and local def-f!
      (is (= 3 (eval-f '((fn [x] (+ x 1)) 2))))

      (is (= 3 (eval-f '((fn [] 3)))))
      (is (= -1 (eval-f '((fn []
                            (def-f! sum (fn [x y] (- x y)))
                            (* 2 8)
                            (sum 2 3))))))
      ;; in the main scope sum is the same
      (is (= 5 (eval-f '(sum 2 3))))
      ;;
      ;; recursion
      (eval-f '(def-f! factorial (fn [n] (if (< n 2)
                                           1
                                           (* n (factorial (- n 1)))))))
      (is (= 1 (eval-f '(factorial 1))))
      (is (= 2 (eval-f '(factorial 2))))
      (is (= 6 (eval-f '(factorial 3))))))

  (testing "misc"
    (let [table (atom {})]
      (install! table 'foo 'self-eval (fn [x] (* x 2)))
      (is (= 4 ((dispatcher table 'foo) 2))))))

(test-multi-table)
