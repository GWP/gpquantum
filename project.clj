(defproject gpquantum "0.1.0-SNAPSHOT"
  :description "FIXME: write this!"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "1.7.170"]
                 [org.clojure/core.async "0.2.374"
                  :exclusions [org.clojure/tools.reader]]
                 [reagent "0.5.1"]
                 [net.mikera/core.matrix "0.49.0"]
                 [complex "0.1.2"]
                 [net.mikera/clojure-utils "0.6.2"] ;was 0.6.1
                 [net.mikera/core.matrix.stats "0.6.0"]
                 [org.clojure/tools.trace "0.7.9"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [incanter "1.5.7"]]

 ; :plugins [[lein-figwheel "0.5.0-5"]
            ;[lein-cljsbuild "1.1.2" :exclusions [[org.clojure/clojure]]]
            ;]
  :main gpquantum.core
  :source-paths ["src"])
