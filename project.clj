(defproject vindinium "0.1.0-SNAPSHOT"
  :description "Starter pack for the vindinium AI contest"
  :url "http://example.com/FIXME"
  :license {:name "MIT"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [clj-http "0.7.8"]
                 [cheshire "5.3.1"]
                 [slingshot "0.10.3"]
                 [org.clojure/data.priority-map "0.0.5"]
                 [org.clojure/core.match "0.2.1"]]
  :jvm-opts ["-Dserver=http://localhost:9000" "-Dkey=bots/test.txt"]
  :main ^:skip-aot vindinium.core
  :profiles {:uberjar {:aot :all}})
