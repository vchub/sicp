(defproject sicp "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [pjstadig/humane-test-output "0.10.0"]
                 [org.clojure/core.async "1.3.610"]
                 ]
  :main ^:skip-aot sicp.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}
             :dev { :plugins [[com.jakemccrary/lein-test-refresh "0.24.1"]
                             [lein-cljfmt "0.7.0"]
                             ]
                   :dependencies [
                                  [lein-cljfmt "0.7.0"]
                               [cider/cider-nrepl "0.25.5"]
                                  [org.clojure/math.combinatorics "0.1.5"]
                                  [org.clojure/test.check "1.1.0"]]}})
