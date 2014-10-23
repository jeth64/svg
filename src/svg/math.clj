(ns svg.math)


(defn round
  ([decimal-places n] (read-string (format (str "%." decimal-places "f") (double n))))
  ([n] (round 0 n)))

(defn factorial [n]
  (apply * (range 1 (inc n))))

(defn choose [n k]
  (if (< k 0)
    0
    (/ (apply * (range (inc (- n k)) (inc n)))
       (apply * (range 1 (inc k))))))


;;
;; Vector operations
;;

(defn- v-piecewise [f v v-c]
  (if (sequential? v-c)
    (mapv f v v-c)        ;vector
    (mapv #(f % v-c) v))) ;constant

(defn v-add [x y] (v-piecewise + x y))

(defn v-sub [x y] (v-piecewise - x y))

(defn v-div [x y] (v-piecewise / x y))

(defn v-mul [x y] (v-piecewise * x y))

(defn v-neg [v] (mapv - v))

(defn v-round
  ([v] (v-round 0 v))
  ([decimal-places v] (mapv (partial round decimal-places) v)))


;;
;; Linear algebra
;;

(defn normalize [x] (Math/sqrt (dot x x)))

(defn dot [x y] (apply + (mapv * x y)))

(defn cross [x y]
  (case (count x)
    2 (apply - (v-mul x (rseq y)))
    3 (let [[x1 x2 x3] x
            [y1 y2 y3] y]
        [(- (* x2 y3) (* x3 y2))
         (- (* x3 y1) (* x1 y3))
         (- (* x1 y2) (* x2 y1))])
    nil))

(defn solve-ls-2d
  "Solve linear system in a two dimensional space using Cramer's rule"
  [[[a1 b1] [a2 b2]] [c1 c2]]
  [(/ (- (* c1 b2) (* b1 c2))
      (- (* a1 b2) (* b1 a2)))
   (/ (- (* a1 c2) (* c1 a2))
      (- (* a1 b2) (* b1 a2)))])


;;
;; Matrix operations
;;

(defn transpose [m] (apply mapv vector m))

(defn- m-piecewise [f m m-v-c]
  (if (sequential? m-v-c)
    (if (sequential? (first m-v-c))
      (mapv (partial mapv f) m m-v-c)      ;matrix
      (mapv #(mapv f % m-v-c) m))          ;vector
    (mapv (partial mapv #(f % m-v-c)) m))) ;constant

(defn m-add [x y] (m-piecewise + x y))

(defn m-sub [x y] (m-piecewise - x y))

(defn mp-div [x y] (m-piecewise / x y))

(defn mp-mul [x y] (m-piecewise * x y))

(defn m-mul [x y]
  (if (sequential? (first x))
    (if (sequential? (first y))
      (mapv (partial mapv (partial dot) x) (transpose y))
      (mapv (partial dot y) x))
    (if (sequential? (first y))
      (mapv (partial dot x) (transpose y))
      (dot x y))))

(defn m-round
  ([decimal-places m] (mapv (partial mapv (partial round decimal-places)) m))
  ([m] (m-round 0 m)))


;;
;; Statistic operations
;;

(defn weighted-sum [data weights]
  (mapv #(dot weights %) (transpose data)))

(defn mean [data]
  (mapv #(/ % (count data))
       (apply mapv + data)))
