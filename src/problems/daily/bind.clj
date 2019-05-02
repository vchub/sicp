(ns problems.daily.bind)

(comment
  (subvec [:a :b :c :d] 1 3)
  (assoc [:a :b :c :d] 1 :x)

  (let [out-f (clojure.java.io/writer "foo.txt")]
    (do (binding [*out*  out-f]
          (dotimes [i 10]
            (prn i, "foo")))
        (.close out-f))))
