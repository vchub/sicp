(ns sicp.ch41.eval-start-test
  (:require [clojure.test :refer :all]
            [sicp.ch41.eval-start :as eval1]))

; (do
;   (def x 1)
;   (def x 2)
;   x)

(deftest env-test
  (let [ evall (fn[exp] (eval1/evall exp eval1/global-env)) ]
    ; (prn @env)
    (testing "def and variable"
      (is (= 1 (evall '(do
                         (def x 1)
                         x)))
          )
      ; (is (= 3 (evall '(do
      ;                    (def x 1)
      ;                    (def y 2)
      ;                    (+ x y))))
      ;     )
      )
    (testing "primitives"
     (is (= 1 (evall 1)))
     (is (= "foo" (evall "foo")))
     ; (is (= 's (evall 's)))
     (is (= 2 (evall (quote (+ 1 1)))))
     (is (= 2 (evall '(+ 1 1))))
     (is (= 2 (evall '(* 2 1))))
     (is (= 1/2 (evall '(/ 1 2))))
     (is (= true (evall '(< 1 2))))
     (is (= true (evall '(= 1 1))))
     (is (= true (evall '(= "a" "a"))))
     (is (= false (evall '(= "b" "a"))))
     (is (= 6 (evall '(* (+ 2 1) (/ 2 1)))))
     )
    ))
