(ns problems.daily.parentheses-test
  (:require [clojure.test :refer :all]
            [problems.daily.parentheses :refer :all]))

(deftest test-balanced?
  (testing "in-keys"
    (is (= (first "()") (in-keys (first "()"))))
    (is (= (first ")") (in-vals (first ")()")))))

  (testing "balanced?"
   (is (= false (balanced? "(")))
   (is (= true (balanced? "()")))
   (is (= true (balanced? "(())")))
   (is (= true (balanced? "(())[]")))
   (is (= true (balanced? "((ab)c )[]")))
   (is (= true (balanced? "mb((ab)c )[]")))
   (is (not= true (balanced? "mb(ab)c )[]")))
   (is (= true (balanced? "mb(x(ab)c )[]lf")))
   (is (not= true (balanced? "mb(x(ab)c ()[]lf")))
   )
  )
