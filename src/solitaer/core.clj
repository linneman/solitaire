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

(defn init-test []
  (let [field (vec (repeat 49 :1))]
    (assoc field 0 :x 1 :x 5 :x 6 :x 7 :x 8 :x 12 :x 13 :x
           35 :x 36 :x 40 :x 41 :x 42 :x 43 :x 47 :x 48 :x
           24 :x)))

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
;(all-next-moves (init-test))
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

(def init-const (initial-constellation (init-field)))
(def const2 (constellations-for-all-moves init-const))
(def const3 (mapcat #(constellations-for-all-moves (:field %)) const2))
(def const3 (mapcat #(constellations-for-all-moves %) const2))


(defn eval-constellation-by-center-distance
  "evaluates the winning changec of a given constellation by calculating
  the distance of each gaming piece to the center field"
  [field]
  (let [distance-index-center
        (fn [index]
          (let [[x y] (index2xy index)]
            (+ (Math/abs (- x 3)) (Math/abs (- y 3)))))
        tile-index-pairs (partition 2 (interleave field (range 49)))
        occupied-tile_index-pairs (filter #(= :1 (first %)) tile-index-pairs)]
    (reduce + (mapcat #(rest %) occupied-tile_index-pairs))
    ))


(def initial (init-field))
(def iter0 (all-next-moves initial))
(def iter1 (mapcat #(all-next-moves (:next-field %)) iter0))
(def iter2 (mapcat #(all-next-moves (:next-field %)) iter1))
(def iter3 (mapcat #(all-next-moves (:next-field %)) iter2))
(def iter4 (mapcat #(all-next-moves (:next-field %)) iter3))



(defn all-next-iterations
  "evaluates n iterations of all possible moves
  for a given iteration"
  [iteration n]
  (if (pos? n)
    (recur (mapcat #(all-next-moves (:next-field %)) iteration) (dec n))
    iteration))



;(eval-constellation-by-center-distance step4)

;(def iter4-eval (map #(assoc % :evalres (eval-constellation-by-center-distance (:next-field %))) iter4))
;(sort-by #(:evalres %) iter4-eval)

(defn sort-by-weighting [iteration]
  (let [iter-plus-evalres (map #(assoc % :evalres (eval-constellation-by-center-distance (:next-field %))) iteration)]
    (sort-by #(:evalres %) iter-plus-evalres)))



(defn search []
  (let [start (all-next-moves (init-field))
        iterations-before-pruning 4]
    (loop [iter start, move-no 1]
      (let [next-n-iter (all-next-iterations iter iterations-before-pruning)
            next-n-iter-pruned (take 10 (sort-by-weighting next-n-iter))]
        (do
          (println (format "analyzed up to move number %d" move-no))
          (if (empty? next-n-iter-pruned)
          iter
          (recur next-n-iter-pruned (+ move-no iterations-before-pruning))))))))





;(all-next-iterations iter0 4))


(defn test-field []
  (let [field (vec (repeat 49 :0))]
    (assoc field 0 :x 1 :x 5 :x 6 :x 7 :x 8 :x 12 :x 13 :x
           35 :x 36 :x 40 :x 41 :x 42 :x 43 :x 47 :x 48 :x
           24 :1 23 :1)))

(def t_iter0 (all-next-moves (test-field)))
(all-next-iterations t_iter0 4)

