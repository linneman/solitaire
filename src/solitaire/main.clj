; Solver for the Game of Solitaire
;
; main class for generation of stand-alone app
;
; by Otto Linnemann
; (C) 2011, GNU General Public Licence

(ns solitaire.main
  (:gen-class)
  (:use [solitaire.core :only (search print-results)] :reload)
  )


; command line interface (leiningen)
(defn -main [& args]
  (let [iterations-str (if (> (count args) 0) (nth args 0) "3")
        iterations (if iterations-str (read-string iterations-str) 3)
        pruning-str  (if (> (count args) 1) (nth args 1) "10")
        pruning (if pruning-str (read-string pruning-str) 10)]
    (do
      (println 
        "Solver for the Game of Solitaire\n
        invocation:  java -jar solitaire-standalone.jar [iterations-before-pruning] [prune-factor]\n
        (C) 2011, GNU General Public Licence by Otto Linnemann\n\n")
      (println
        (format "Starting application with %d iterations before pruning to %d constellations..."
                iterations
                pruning
                ))
      (time (def res (search iterations pruning)))
      (dorun (print-results res))
      )))

