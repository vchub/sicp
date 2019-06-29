(ns sicp.ch3.spec-tutor
  (:require [clojure.test :refer :all]
            [clojure.spec.alpha :as s]))

(s/def ::suit #{:club :diamond :heart :spade})
(s/def ::name-or-id (s/or :name string?
                          :id int?))

(deftest test-examples
  (is (s/conform even? 3))
  (is (s/valid? even? 2))
  (is (s/valid? string? "a"))
  (is (s/valid? #(> % 5) 10))
  (is (not (s/valid? #(> % 5) 2)))
  (is (s/valid? #{:club :diamond :heart :spade} :club)) ;; true
  (is (s/valid? ::suit :club)) ;; true
  (is (not (s/valid? #{:club :diamond :heart :spade} 42))) ;; false
  (is (s/valid? #{42} 42)) ;; true
  ;; (s/explain ::suit 42)
  (is (= [:id 42](s/conform ::name-or-id 42))) ;; true
  )

(test-examples)
