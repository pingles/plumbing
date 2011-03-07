(defproject clj-sys/plumbing "0.1.3-SNAPSHOT"
  :description "General purpose functions for clj-sys projects"
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [woven/clj-json "0.3.2"]
                 [log4j/log4j "1.2.16"]
                 [commons-io "2.0.1"]]
  :dev-dependencies [[swank-clojure "1.3.0-SNAPSHOT"]
                     [lein-clojars "0.5.0"]]
  :repositories  {"apache" "https://repository.apache.org/content/repositories/releases/"
                  "snapshots" "http://mvn.getwoven.com/repos/woven-public-snapshots"
                  "releases" "http://mvn.getwoven.com/repos/woven-public-releases"})
