(defproject aoc18 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [better-cond "1.0.1"]
                 [org.clojure/math.combinatorics "0.1.4"]
                 [clj-time "0.15.0"]]
  :main ^:skip-aot aoc18.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
