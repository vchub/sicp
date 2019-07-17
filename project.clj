(defproject sicp "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [pjstadig/humane-test-output "0.9.0"]
                  [org.clojure/core.async "0.4.490"]
                 ]
  :main ^:skip-aot sicp.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}
             :dev { :plugins [[com.jakemccrary/lein-test-refresh "0.23.0"]
                             [lein-cljfmt "0.6.3"]
                             ]
                   :dependencies [[lein-cljfmt "0.6.3"]
                                  [org.clojure/math.combinatorics "0.1.5"]
                                  [org.clojure/test.check "0.10.0-RC1"]]}})
