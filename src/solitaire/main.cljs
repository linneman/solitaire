; Solver for the Game of Solitaire
;
; main class for generation of stand-alone app
;
; by Otto Linnemann
; (C) 2011, GNU General Public Licence
; Minor changes to allow compilation as ClojureScript by Jörg Ramb

(ns solitaire.main
  (:require [solitaire.core :as sc])
  )

(def str2int js/parseInt) ; added for ClojureScript

; command line interface (leiningen)
(defn -main [& args]
  (let [iterations-str (if (> (count args) 0) (first args) "3")
        iterations (if iterations-str (str2int iterations-str) 3)
        pruning-str  (if (> (count args) 1) (second args) "10")
        pruning (if pruning-str (str2int pruning-str) 10)
        ]
    (println
      "Solver for the Game of Solitaire (ClojureScript version)\n
      invocation:  node solitaire.js [iterations-before-pruning] [prune-factor]\n
      (C) 2011, GNU General Public Licence by Otto Linnemann\n\n")
    (println "Starting application with" iterations "iterations before pruning to" pruning "constellations...")
    (let [res (time (def res (sc/search iterations pruning)))]
      (dorun (sc/print-results res)))
    ))

(set! *main-cli-fn* -main)
