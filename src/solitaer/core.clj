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
  #^{:doc "moves piece at position index to the north"
     :id "north"}
  move-to-north [field index]
  (let [dest-index (- index 14)
        leapt-index (- index 7)]
    (if (and (>= dest-index 0) (= (field index) :1) (= (field leapt-index) :1) (= (field dest-index) :0))
      (assoc field dest-index :1 index :0 leapt-index :0)
      field)))

(defn
  #^{:doc "moves piece at position index to the south"
     :id "south"}
  move-to-south [field index]
  (let [dest-index (+ index 14)
        leapt-index (+ index 7)]
    (if (and (< dest-index 49) (= (field index) :1) (= (field leapt-index) :1) (= (field dest-index) :0))
      (assoc field dest-index :1 index :0 leapt-index :0)
      field)))

(defn
  #^{:doc "moves piece at position index to the east"
     :id "east"}
  move-to-east [field index]
  (let [[x y] (index2xy index) 
        dest-x (+ x 2)
        dest-index (+ index 2)
        leapt-index (+ index 1)]
    (if (and (< dest-x 8) (= (field index) :1) (= (field leapt-index) :1) (= (field dest-index) :0))
      (assoc field dest-index :1 index :0 leapt-index :0)
      field)))

(defn
  #^{:doc "moves piece at position index to the west"
     :id "west"}
  move-to-west [field index]
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
                     {:from index :dir (:id (meta move)) :next-field next-field}
                     ))) (range 49))))

;(next-moves move-to-north step4)

(defn all-next-moves [field]
  (mapcat #(next-moves % field) [move-to-north move-to-south move-to-east move-to-west]))

;(all-next-moves step4)
;(all-next-moves (init-test))


(def initial (init-field))
(def iter0 (all-next-moves initial))
(def iter1 (mapcat #(all-next-moves (:next-field %)) iter0))
(def iter2 (mapcat #(all-next-moves (:next-field %)) iter1))
(def iter3 (mapcat #(all-next-moves (:next-field %)) iter2))
(def iter4 (mapcat #(all-next-moves (:next-field %)) iter3))



(defn eval-constellation-by-center-distance [field]
  "evaluates the winning changec of a given constellation by calculating
  the distance of each gaming piece to the center field"
  (let [distance-index-center
        (fn [index]
          (let [[x y] (index2xy index)]
            (+ (Math/abs (- x 3)) (Math/abs (- y 3)))))
        tile-index-pairs (partition 2 (interleave field (range 49)))
        occupied-tile_index-pairs (filter #(= :1 (first %)) tile-index-pairs)]
    (reduce + (mapcat #(rest %) occupied-tile_index-pairs))
    ))

;(eval-constellation step4)
