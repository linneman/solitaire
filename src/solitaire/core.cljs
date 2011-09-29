; Solver for the Game of Solitaire
;
; (C) 2011 GNU General Public Licence
; Otto Linnemann

(ns solitaire.core
  #_(:require [clojure.contrib.math :as math]))


(defn abs [n]
  (if (> n 0) n -n))

; ----- board definition and basic moves -----

(defn init-field []
  (let [field (vec (repeat 49 :1))]
    (assoc field 0 :x 1 :x 5 :x 6 :x 7 :x 8 :x 12 :x 13 :x
           35 :x 36 :x 40 :x 41 :x 42 :x 43 :x 47 :x 48 :x
           24 :0)))

(defn toString [field]
  (let [convf (fn [a] (map #(case % :0 "0" :1 "X" :n "\n" " ") (cons :n a)))]
    (reduce str (flatten (map convf (partition 7 field))))))

(defn index2xy [index]
  [(rem index 7) (quot index 7)])

(defn
  move-to-north
  "moves piece at position index to the north"
  [field index]
  (let [dest-index (- index 14)
        leapt-index (- index 7)]
    (if (and (>= dest-index 0) (= (field index) :1) (= (field leapt-index) :1) (= (field dest-index) :0))
      (assoc field dest-index :1 index :0 leapt-index :0)
      field)))

(defn
  move-to-south
  "moves piece at position index to the south"
  [field index]
  (let [dest-index (+ index 14)
        leapt-index (+ index 7)]
    (if (and (< dest-index 49) (= (field index) :1) (= (field leapt-index) :1) (= (field dest-index) :0))
      (assoc field dest-index :1 index :0 leapt-index :0)
      field)))

(defn
  move-to-east
  "moves piece at position index to the east"
  [field index]
  (let [[x y] (index2xy index) 
        dest-x (+ x 2)
        dest-index (+ index 2)
        leapt-index (+ index 1)]
    (if (and (< dest-x 7) (= (field index) :1) (= (field leapt-index) :1) (= (field dest-index) :0))
      (assoc field dest-index :1 index :0 leapt-index :0)
      field)))

(defn
  move-to-west
  "moves piece at position index to the west"
  [field index]
  (let [[x y] (index2xy index) 
        dest-x (- x 2)
        dest-index (- index 2)
        leapt-index (- index 1)]
    (if (and (>= dest-x 0) (= (field index) :1) (= (field leapt-index) :1) (= (field dest-index) :0))
      (assoc field dest-index :1 index :0 leapt-index :0)
      field)))

; usage illustration for field setup
 (def initial (init-field))
; (def step1 (move-to-north initial 38))
; (def step2 (move-to-south step1 17))
; (def step3 (move-to-east step2 22))
; (def step4 (move-to-west step3 25))
; 
; (println (toString initial))
; (println (toString step1))
; (println (toString step2))
; (println (toString step3))
; (println (toString step4))

(defn next-moves [move field]
  (filter #(not= % nil)
          (map (fn [index]
                 (let [next-field (move field index)]
                   (if (not= field next-field)
                     {:from index :dir move :next-field next-field}
                     ))) (range 49))))

(defn all-next-moves [field]
  (mapcat #(next-moves % field) [move-to-north move-to-south move-to-east move-to-west]))


; ----- search all (many) constellations -----

(defn initial-constellation
  "Creates a new constellation hash-map for a given
  game board vector. The idea is here to eliminate same
  positions, so we use them as key in a hash map in order
  to ensure that each position is stored and evaluated only
  once."
  [field]
  (hash-map field { :move (list) }))

; usage illustration
; (def a (hash-map :pos1 "info1" :pos2 "info2" :pos3 "info3"))
; (def b (hash-map :pos4 "info1" :pos5 "info2" :pos2 "info33"))
; (merge a b)

(defn constellations-for-move
  "Takes a movement function and a constellation hash-map
  with only one element. Its key is the evaluated board setup,
  its value is additional information as the moves applied
  to get to the current position, optimization data, etc.
  The function returns a new hash-map with all reachable
  configurations by the given move"
  [move constellation]
  ; {:pre [(= (count (keys constellation)) 1) (= (count (vals constellation)) 1)]}
  (apply
    hash-map
    (mapcat (fn [index]
              (let [field (first (keys constellation))
                    next-field (move field index)]
                (if (not= field next-field)
                  [next-field
                   {:move (conj (:move (first (vals constellation))) {:from index :dir move})}]
                  ))) (range 49))))



(defn map-and-merge-on-map-entries
  "Iterates each key in the hash map m and evaluates
  function f for it which is required to deliver one
  or several hash-maps of the same type as m as result.
  All generated hash-entries are merged in the result
  hash and returned."
  [f m]
  (reduce #(merge %1 (f (apply hash-map %2))) {} m))


; usage illustration
; (def x (map-and-merge-on-map-entries #(identity %) a))
; (prn x)


(defn constellations-for-all-moves
  "Takes a constellation hash-map with several key value pairs.
  The key provide evaluated board setups, the values provide
  additional information as the moves applied to get to a
  given setup, optimization data, etc.
  The function returns a new hash-map with all reachable
  configurations from the given configuration"
  [constellations]
  ; {:pre [(isa? (class constellations) clojure.lang.APersistentMap)]}
  (reduce merge (map
                  (fn [dir] 
                    (map-and-merge-on-map-entries
                      (partial constellations-for-move dir) constellations))  
                  [move-to-north move-to-south move-to-east move-to-west]         
                  )))

; usage illustration
; (def init-const (initial-constellation (init-field)))
; (prn init-const)
; 
; (def const2 (constellations-for-all-moves init-const))
; (prn const2)
; 
; (def const3 (constellations-for-all-moves const2))
; (prn const3)


; ----- score functions for pruning the search tree -----

(defn eval-field-by-center-distance
  "evaluates the winning changec of a given constellation by calculating
  the distance of each gaming piece to the center field"
  [field]
  (let [distance-index-center
        (fn [index]
          (let [[x y] (index2xy index)]
            (+ (Math/abs (- x 3)) (Math/abs (- y 3)))))
        field-factors (map #(distance-index-center %) (range 49))
        tile-index-pairs (partition 2 (interleave field field-factors))
        occupied-tile_index-pairs (filter #(= :1 (first %)) tile-index-pairs)]
    (reduce + (mapcat #(rest %) occupied-tile_index-pairs))
    ))


(defn weighting-field-factors []
  (let [movs_4 1 
        movs_2 2
        movs_1 4
        field (vec (repeat 49 movs_4))]
    (assoc field
           2 movs_2 3 movs_1 4 movs_2
           9 movs_2 10 movs_1 11 movs_2

           14 movs_2 15 movs_2
           21 movs_1 22 movs_1
           28 movs_2 29 movs_2

           19 movs_2 20 movs_2
           26 movs_1 27 movs_1
           33 movs_2 34 movs_2

           37 movs_2 38 movs_1 39 movs_2
           44 movs_2 45 movs_1 46 movs_2
           )))


(defn eval-field-by-movability
  "evaluates the winning changec of a given constellation by determinng
  the amount of maximum possible moves for each piece. In practise
  this function provides not so good results in comoparison to the
  simple distance function."
  [field]
  (let [tile-index-pairs (partition 2 (interleave field (weighting-field-factors)))
        occupied-tile_index-pairs (filter #(= :1 (first %)) tile-index-pairs)]
    (reduce + (mapcat #(rest %) occupied-tile_index-pairs))
    ))


; ----- recursive iteration for building search tree  -----

(defn all-next-iterations
  "evaluates n iterations of all possible moves
  for a given iteration"
  [iteration n]
  (let [next-iteration (constellations-for-all-moves iteration)]
    (if (and (pos? n) (seq next-iteration))
      (recur next-iteration (dec n))
      [iteration n])))


; (def a (def next-n-iter (all-next-iterations init-const 4)))
; (def c (all-next-iterations init-const 4))
; (eval-field-by-center-distance step4)
; (eval-field-by-movability step4)


(defn map-function-on-map-vals
  "utility function for altering a hash-map with
  a function which takes the key value pair for
  each hash entry."
  [m f]
  (reduce (fn [altered-map [k v]] (assoc altered-map k (f k v))) {} m))


(defn prune-constellations
  "reduces number of constellations selecting only
  the amount given in prune-factor with the lowest
  score value." 
  [constellations prune-factor]
  (let [iter-plus-evalres
        (map-function-on-map-vals constellations
                                  (fn [k v] (assoc v :evalres (eval-field-by-center-distance k))))
        sorted-score-field-pairs
        (sort-by first (map #(vector (:evalres (val %)) (key %))
                      iter-plus-evalres))
        sorted-hashes (map #(second %) sorted-score-field-pairs)
        pruned-hashes (take prune-factor sorted-hashes)]
    (select-keys iter-plus-evalres pruned-hashes)))



; ----- entry function for starting the search  -----

(defn search
  "start the search with initial game board.
  Two arguments determine the number of searched positions:
  With <iterations-before-pruning> the number of complete
  build iterations is determined. After this pruning takes
  place which means that only <prune-factor> iterations
  with the most promising score taken into the further
  evaluation."
  [iterations-before-pruning prune-factor]
  (let [start (initial-constellation (init-field))]
    (loop [iter start, move-no 1]
      (let [[next-n-iter n] (all-next-iterations iter iterations-before-pruning)
            next-n-iter-pruned (prune-constellations next-n-iter prune-factor)]
        (do
          (println (format "analyzed up to move number %d" move-no))
          (if (pos? n)
          next-n-iter
          (recur next-n-iter-pruned (+ move-no (- iterations-before-pruning n)))))))))

; usage illustration
; (distance-function, 24s, 4 constellation with 1 pieces left found, one is solution )
; (time (def res (search 3 10)))
; (distance-function, 48s, 6 constellation with 1 pieces left found, one is solution )
; (time (def res (search 3 20))) 



; ----- output of result data -----

(defn count-remaining-stones
  "returns a vector with the number of remaining
  stones for each constellation"
  [constellations]
  (let [fields (keys constellations)
        count-pieces (fn [field] (count (filter #(= :1 %) field)))]
    (map count-pieces fields)))


(defn print-final-boards
  "displays the final board for each constellation"
  [constellations]
  (let [fields (keys constellations)]
    (map #(println (toString %)) fields)))


(defn move-list-to-string
  "prints the move-list in the format
  stored in constellation."
  [rev-moves]
  (let [moves (reverse rev-moves)
        dirfcnt2index
        (fn [fcn]
          (cond
            (= fcn move-to-north) -14
            (= fcn move-to-south) 14
            (= fcn move-to-east)  2
            (= fcn move-to-west) -2
            :else nil
            ))
        native-coord
        (map  #(list (:from %) (+ (:from %) (dirfcnt2index (:dir %)))) moves)
        std-coord-ind [:x :x  1  2  3 :x :x 
                       :x :x  4  5  6 :x :x
                       7  8  9 10 11 12 13
                       14 15 16 17 18 19 20
                       21 22 23 24 25 26 27
                       :x :x 28 29 30 :x :x
                       :x :x 31 32 33 :x :x]
        n2s (fn [native] (std-coord-ind native))
        std-corrd (map
                    #(list (n2s (:from %))
                           (n2s (+ (:from %) (dirfcnt2index (:dir %)))))
                    moves)]
    std-corrd))

(defn print-results
  "displays the move list and the final board for each constellation"
  [constellations]
  (map 
    #(println
      (move-list-to-string (:move (val %))) "\n"
      (toString (key %)) "\n\n-------\n")
    constellations))


; ----- sample invocation for starting solver and printing results -----

;(time (def res (search 3 10)))
;(print-results res)
 
