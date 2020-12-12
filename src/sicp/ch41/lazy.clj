(ns sicp.ch41.lazy
  (:require [sicp.ch41.eval-start :as start]))

(def global-env (atom (into {} @start/global-env)))

(defn evall [exp] (start/evall exp global-env))

(defn it! [tag proc]
  (start/install! global-env 'evall tag proc))

(it! 'unless (fn [exp env] (if (evall (second exp))
                             (evall (nth exp 3))
                             (evall (nth exp 2)))))
