(defproject cljs-blackjack "0.1.0-SNAPSHOT"
  :description "Clojurescript blackjack program that uses Reagent."
  :url "http://example.com/FIXME"
  :dependencies [[org.clojure/clojure "1.7.0-beta2"]
                 [org.clojure/clojurescript "0.0-3211"]
                 [reagent "0.5.0"]
                 [domina "1.0.3"]]
  :jvm-opts ^:replace ["-Xmx1g" "-server"]
  :node-dependencies [[source-map-support "0.2.8"]]
  :plugins [[lein-npm "0.4.0"]]
  :source-paths ["src" "target/classes"]
  :clean-targets ["out" "release" "target" ".repl-0.0-3211"]
  :target-path "target")
