(ns sicp.ch4.debug-tutor
  (:require [clojure.test :refer :all]))

(defn foo
  [n]
  (cond (> n 40) (+ n 20)
        (> n 20) (- (first n) 20)
        :else 0))

(comment
;;  to throw away all the potentially useful information: the exception classes, the stacktraces, and the error messages. To do this I will start a subrepl that snidely elides all information about exceptions caught by the REPL:
  (require '[clojure.main :as main])
  (main/repl :caught (fn [_] (println "Broken! HaHa!")))

  (def n 24)
  (foo 24)
  )
