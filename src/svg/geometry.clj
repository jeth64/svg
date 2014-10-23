(ns svg.geometry
  (:require [svg.math :refer :all]))


(defn rotation-matrix [degree]
  (let [radian (Math/toRadians degree)
        c (Math/cos radian)
        s (Math/sin radian)]
    [[c s] [(- s) c]]))

(defn rotate
  "Rotate given vectors by degree around center"
  ([degree vectors] (rotate degree vectors [0 0]))
  ([degree center vectors]
     (m-add (m-mul (m-sub vectors center)
                   (transpose (rotation-matrix degree)))
            center)))

(defn cross-line ; check if faster with 'rotate'
  "Returns line rotated around midpoint by 90 degree"
  [line]
  (let [[midpt (mean line) ]
        [rotated-dir (dot [[0 -1] [1 0]]
                          (apply - line))]]
    [(+ midpt (/ rotated-dir 2))
     (- midpt (/ rotated-dir 2))]))

(defn stretch-line
  "Returns a stretched version of the given line
   - 'mode':
     - :right  :: The extension of the line occurs in the direction of the first given point.
                  The second point stays the same.
       :left   :: The opposite of :right
       :middle :: The line stretches uniformly to both sides"
  ([line factor mode]
     (case mode
       :right [(first line)
               (v-add (first line)
                      (v-mul (apply v-sub line)
                             (- factor)))]
       :left [(v-add (second line)
                     (v-mul (apply v-sub line)
                            factor))
              (second line)]
       :middle (let [midpoint (mean line)]
                 [(v-add midpoint
                         (v-mul (apply v-sub line)
                                (- (/ factor 2))))
                  (v-add midpoint
                         (v-mul (apply v-sub line)
                                (/ factor 2)))]))))


;; http://www.intmath.com/matrices-determinants/1-determinants.php


(defn intersection
  "Returns intersection of lines or nil if parallel"
  [line1 line2]
  (try (solve-ls-2d (transpose [(v-sub (last line1) (first line1))
                                (v-sub (first line2) (last line2))])
                    (v-sub (first line2) (first line1)))
       (catch Exception e nil)))

(defn bounding-box [points dir-vec]
  "Returns vertices of rotated rectangle containing given points.
   The direction is given by a normalized vector"
  (let [degree (Math/acos (dot [0 1] dir-vec))
        p0 (mean points)
        normalized (m-sub points p0)
        rotated (rotate degree normalized)
        xs (map first rotated)
        ys (map second rotated)]
    (m-add (rotate (- degree)
                   (for [x [(apply min xs) (apply max xs)]
                         y [(apply min ys) (apply max ys)]]
                     [x y]))
           p0)))


(defn points-in-triangle? [vertices points]
  "States if all given points are within the triangle defined by the vertices"
  (let [[v-dirs (map v-sub vertices (rest (cycle vertices)))]
        [v2-dirs (map v-sub vertices (rest (cycle vertices)))]
        [p-dirs (map (fn [z] (map (fn [x] (v-sub z x)) points)) vertices)]]
    (apply and (flatten (map (fn [x y z] (map (fn [p] (= (pos? (cross x y))
                                                       (pos? (cross x p))))
                                             z))
                             v-dirs v2-dirs p-dirs)))))
