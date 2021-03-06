; Solver for the Game of Solitaire
;
; main class for generation of stand-alone app
;
; by Otto Linnemann
; (C) 2011, GNU General Public Licence

(ns solitaire.main
  (:gen-class)
  (:require [cider.nrepl :refer (cider-nrepl-handler)]
            [clojure.tools.nrepl.server :as nrepl-server])
  (:use [solitaire.core :only (search print-results)] :reload)
  )


; command line interface (leiningen)
(defn -main [& args]
  (let [iterations-str (or (nth args 0) "3")
        iterations (read-string iterations-str)
        pruning-str (or (nth args 1) "10")
        pruning (read-string pruning-str)]
    (do
      ;(nrepl-server/start-server :port 7888 :handler cider-nrepl-handler)
      (println
        "Solver for the Game of Solitaire\n
        invocation:  java -jar solitaire-standalone.jar [iterations-before-pruning] [prune-factor]\n
        (C) 2011, GNU General Public Licence by Otto Linnemann\n\n")
      (println
        (format "Starting application with %d iterations before pruning to %d constellations..."
                iterations
                pruning
                ))
      (let [res (time (doall (search iterations pruning)))]
        (dorun (print-results res))
        (System/exit 0)))))
