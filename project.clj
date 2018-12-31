(defproject advent-of-code "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [spyscope "0.1.6"]
                 [clojure.java-time "0.3.2"]]
  :profiles {:dev  {:resource-paths ["src/advent_of_code/resources"]}
             :user {:dependencies [[spyscope "0.1.6"]]
                    :injections   [(require 'spyscope.core)]}})
