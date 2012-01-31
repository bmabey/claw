(defproject claw "0.1.0"
  :description "claw - Clojure Linear Algebra Workbench"
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [org.clojure/core.incubator "0.1.0"]
                 [incanter/parallelcolt "0.9.4"]]
  :dev-dependencies [[midje "1.3.1" :exclusions [org.clojure/clojure]]
                     [lein-midje "1.0.7"]]
  :warn-on-reflection true)
