; Solver for the Game of Solitaire
;
; main class for generation of stand-alone app
;
; by Otto Linnemann
; (C) 2011, GNU General Public Licence

(ns solitaire.main
  (:require [pixie.time :refer [time]]
            [solitaire.core :refer [search print-results]]))

;(swap! load-paths conj "/Users/ol/Entwicklung/clojure/solitaire/src/")
;(swap! load-paths conj "./src/")


; command line interface (leiningen)
(defn- main [& args]
  (let [iterations-str (or (first args) "3")
        iterations (read-string iterations-str)
        pruning-str  (or (second args) "10")
        pruning (read-string pruning-str)]
    (do
      ;(nrepl-server/start-server :port 7888 :handler cider-nrepl-handler)
      (println
        "Solver for the Game of Solitaire\n
        invocation:  java -jar solitaire-standalone.jar [iterations-before-pruning] [prune-factor]\n
        (C) 2011, GNU General Public Licence by Otto Linnemann\n\n")
      (println
       "Starting application with " iterations
       " iterations before pruning to " pruning " constellations...")
      (let [res (time (search iterations pruning))]
        (println (print-results res))
        0))))

;(apply main ["2" "3"])
(apply main program-arguments)
