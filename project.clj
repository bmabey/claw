(defproject claw "1.0.0-SNAPSHOT"
  :description "claw - Clojure Linear Algebra Workbench"
  :dependencies [;TODO: upgrade to 1.3.. the only dep is on c.c.def
                 [org.clojure/clojure "1.2.1"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [incanter/parallelcolt "0.9.4"]]
  :dev-dependencies [[midje "1.3.0-RC2"]
                     [lein-midje "1.0.3"]])