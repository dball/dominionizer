(defproject dominionizer "0.1.0-SNAPSHOT"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/data.csv "0.1.2"]]
  :main ^:skip-aot dominionizer.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
