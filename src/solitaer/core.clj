; Solver for the Game of Solitaer
; (C) 2011 GNU General Public Licence
; Otto Linnemann

(ns solitaer.core
  (:require [clojure.contrib.math :as math]))


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
    (if (and (< dest-x 8) (= (field index) :1) (= (field leapt-index) :1) (= (field dest-index) :0))
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

(def initial (init-field))
(def step1 (move-to-north initial 38))
(def step2 (move-to-south step1 17))
(def step3 (move-to-east step2 22))
(def step4 (move-to-west step3 25))

(println (toString initial))
(println (toString step1))
(println (toString step2))
(println (toString step3))
(println (toString step4))

(defn next-moves [move field]
  (filter #(not= % nil)
          (map (fn [index]
                 (let [next-field (move field index)]
                   (if (not= field next-field)
                     {:from index :dir move :next-field next-field}
                     ))) (range 49))))

(defn all-next-moves [field]
  (mapcat #(next-moves % field) [move-to-north move-to-south move-to-east move-to-west]))

;(next-moves move-to-north step4)

;(all-next-moves step4)
;
;
;
;
(defn initial-constellation [field]
  { :move (list) :field field })

(defn constellations-for-move [move constellation]
  (filter #(not= % nil)
          (map (fn [index]
                 (let [field (:field constellation)
                       next-field (move field index)]
                   (if (not= field next-field)
                     (assoc constellation
                            :move (conj (:move constellation) {:from index :dir move })
                            :field next-field)
                     ))) (range 49))))

(defn constellations-for-all-moves [const]
  (mapcat #(constellations-for-move % const) [move-to-north move-to-south move-to-east move-to-west]))

(def init-const (vector (initial-constellation (init-field))))
(def const2 (mapcat #(constellations-for-all-moves %) init-const))
(def const3 (mapcat #(constellations-for-all-moves %) const2))


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
  the amount of maximum possible moves for each piece"
  [field]
  (let [tile-index-pairs (partition 2 (interleave field (weighting-field-factors)))
        occupied-tile_index-pairs (filter #(= :1 (first %)) tile-index-pairs)]
    (reduce + (mapcat #(rest %) occupied-tile_index-pairs))
    ))

(defn all-next-iterations
  "evaluates n iterations of all possible moves
  for a given iteration"
  [iteration n]
  (let [next-iteration (apply concat (map #(constellations-for-all-moves %) iteration))]
    (if (and (pos? n) (seq next-iteration))
      (recur next-iteration (dec n))
      [iteration n])))

; (def a (def next-n-iter (all-next-iterations init-const 4)))
; (eval-field-by-center-distance step4)
; (eval-field-by-movability step4)

(defn sort-constellation-by-weighting [iteration]
  (let [iter-plus-evalres (map #(assoc % :evalres (eval-field-by-center-distance (:field %))) iteration)]
    (sort-by #(:evalres %) iter-plus-evalres)))

;(sort-constellation-by-weighting const2)

(defn search [iterations-before-pruning prune-factor]
  (let [start (vector (initial-constellation (init-field)))]
    (loop [iter start, move-no 1]
      (let [[next-n-iter n] (all-next-iterations iter iterations-before-pruning)
            next-n-iter-pruned (take prune-factor (sort-constellation-by-weighting next-n-iter))]
        (do
          (println (format "analyzed up to move number %d" move-no))
          (if (pos? n)
          next-n-iter
          (recur next-n-iter-pruned (+ move-no (- iterations-before-pruning n)))))))))



(time (def res (search 3 10))) ; (distance-function, 64s, 1 pieces left but none in the center)
;(time (def res (search 3 15))) ; (distance-function, 99s, 1 pieces left but none in the center)
;(time (def res (search 3 20))) ; (distance-function, 145s, 1 pieces left but none in the center)
;(time (def res (search 3 25))) ; (distance-function, 171s, 1 pieces left but none in the center)
;(time (def res (search 3 30))) ; (distance-function, 211s, 1 pieces left but none in the center)

;(time (def res (search 4 5))) ; (distance-function, 50 min, 3 pieces left)


;(time (def res (search 3 10))) ; (movability-function, 6s, 4 pieces left)
;(time (def res (search 4 10))) ; (movability-function, 23s, 4 pieces left)
;(time (def res (search 5 10))) ; (takes forever)

;(time (def res (search 4 15))) ; (movability-function, 30s, 4 pieces left)
;(time (def res (search 4 17))) ; (movability-function, 61s, 4 pieces left)
;(time (def res (search 4 20))) ; (movability-function, mover than 20 minutes up to iteration 13)


(map (fn [constellation] 
       (let [field (:field constellation)]
         (count (filter #(= :1 %) field)))) res)

(map (fn [constellation]
       (println (toString (:field constellation)))) res)


;(all-next-iterations const2 4)


(defn test-field []
  (let [field (vec (repeat 49 :0))]
    (assoc field 0 :x 1 :x 5 :x 6 :x 7 :x 8 :x 12 :x 13 :x
           35 :x 36 :x 40 :x 41 :x 42 :x 43 :x 47 :x 48 :x
           24 :1 23 :1)))


