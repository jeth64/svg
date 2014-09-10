(ns svg.bezier)

;;
;; see
;; - http://en.wikipedia.org/wiki/B%C3%A9zier_curve#Quadratic_B.C3.A9zier_curves
;; - http://yamnuska.ca/geek/degreeReductionBezier.pdf


;; general math operations
(defn round
  ([decimal-places n] (read-string (format (str "%." decimal-places "f") (double n))))
  ([n] (round 0 n)))

(defn matrix-round
  ([decimal-places m] (mapv (partial mapv (partial round decimal-places)) m))
  ([m] (matrix-round 0 m)))

(defn transpose [m] (apply mapv vector m))

(defn factorial [n]
  (apply * (range 1 (inc n))))

(defn choose [n k]
  (if (< k 0)
    0
    (/ (apply * (range (inc (- n k)) (inc n)))
       (apply * (range 1 (inc k))))))

(defn weighted-sum [points weights]
  (mapv #(apply + (map * weights %)) (transpose points)))


;; bezier functions

(defn elevate
  ([control-points]
     (apply mapv (fn [pi-1 pi i]
                  (mapv #(+ (* (/ i (count control-points)) %1)
                           (* (- 1 (/ i (count control-points))) %2))
                       pi-1 pi))
            [(cons [0 0] control-points)
             (conj control-points [0 0])
             (range (inc (count control-points)))]))
  ([control-points by-degree]
      (loop [P (vec control-points) i by-degree]
        (if (> i 0)
          (recur (elevate P) (dec i))
          P))))

(defn elevate-high [control-points by-degree]
  (letfn [(summand [i j pj]
            (let [c (/ (* (choose (dec (count control-points)) j)
                          (choose by-degree (- i j)))
                       (choose (+ (dec (count control-points)) by-degree)
                               i))]
              (map (partial * c) pj)))
          (calc-pi [i]
            (reduce (partial map +)
                    (map (partial summand i)
                         (range (count control-points))
                         control-points)))]
    (map calc-pi (range (+ (count control-points) by-degree)))))

(defn reduce-degree [control-points]
  (letfn [(approx-points [points n]
            (rest (reductions (fn [P [i Q]]
                                (weighted-sum [Q P]
                                              [(/ n (- n i)) (/ (- i) (- n i))]))
                              [0 0] (transpose [(range n) points]))))
          (factor [i n]
            (* (Math/pow 2 (- 1 (* 2 n)))
               (reduce #(+ %1 (choose (* 2 n) (* 2 %2)))
                       0 (range (inc i)))))]
    (let [n (dec (count control-points))
          Pr (approx-points control-points n)
          Pl (reverse (approx-points (reverse control-points) n))]
      (map #(let [lambda (factor %3 n)]
              (weighted-sum [%1 %2] [(- 1 lambda) lambda]))
           Pr Pl (range n) ))))

(defn bezier-polynomial
  "Returns coefficients for polynomial form of bezier curve defined by given points
   (bezier-polynomial (range 4)) => (0 0 3 0) for 0*t^3 + 0* t^2 + 3*t^1 + 0*t^0"
 [control-points]
 (-> (map (fn [j] (map (partial * (choose (dec (count control-points)) j))
                      (weighted-sum (take (inc j) control-points)
                                    (map #(* (if (zero? (mod (+ % j) 2)) 1 -1) (choose j %))
                                         (range (inc j))))))
          (range (count control-points)))
     reverse vec))


(defn de-casteljau [t control-points];; test
  (loop [coefs control-points]
    (if (> (count coefs) 1)
      (recur (map #(weighted-sum [%1 %2] [(- 1 t) t])
                  coefs (rest coefs)))
      coefs)))


(defn de-casteljau-split-at [t control-points];; test
  (loop [coefs control-points
         curve1 [(first control-points)]
         curve2 [(last control-points)]]
    (if (> (count coefs) 1)
      (let [new-points (map #(weighted-sum [%1 %2] [(- 1 t) t]) coefs (rest coefs))]
        (recur new-points
               (conj curve1 (first new-points))
               (cons curve2 (last new-points))))
      [curve1 curve2])))
