(defproject solitaire "1.0.0-SNAPSHOT"
            :description "Solver for the Game of Solitaire"
            :dependencies [[org.clojure/clojure "1.6.0"]
                           [org.clojure/math.numeric-tower "0.0.4"]
                           [org.clojure/tools.nrepl "0.2.7"]]
            :plugins [[cider/cider-nrepl "0.9.0-SNAPSHOT"]]
            :main solitaire.main
            :aot [solitaire.main])
