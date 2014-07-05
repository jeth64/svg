(ns svg.bezier-test
  (:require [clojure.test :refer :all]
            [midje.sweet :refer [facts fact]]
            [clojure.test.check :as tc] ;; for random testing
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [svg.bezier :refer :all]))

(defn rand-matrix
  ([n max] (vec (take n (repeatedly #(rand-int max)))))
  ([m n max] (vec (take m (repeatedly #(rand-matrix n max))))))

(defn cubic-bezier-coeffs
  "Alternate implementation for cubic bezier polynomials for comparison"
  [control-points]
  (transpose (map #(vector (apply + (map * [-1 3 -3 1] %)) ;; * t^3
                           (apply + (map * [3 -6 3] %))    ;; * t^2
                           (apply + (map * [-3 3] %))      ;; * t
                           (first %))
                  (transpose control-points))))

(facts "About 'round'"
       (fact "integer rounding succeeds"
             (round 0 2.5) => 3)
       (fact "decimal rounding succeeds"
             (round 1 2.05) => 2.1))

(facts "About 'matrix round'"
       (fact "decimal rounding succeeds"
             (matrix-round 0
                           [[-6.938893903907228E-18 0.0] [7.979727989493313E-17 4.0]
                            [1.9999999999999998 4.0] [2.0 3.0] [1.5 3.0]])
             => [[0 0] [0 4] [2 4] [2 3] [2 3]]))

(facts "About 'elevate'"
       (fact "simple example succeeds"
             (elevate [[0 0] [0 4] [2 4] [2 3] [1.5 3]] 1)
             => [[0 0] [0 (/ 16 5)] [(/ 6 5) 4] [2 (/ 18 5)] [(/ 19 10) 3] [1.5 3]]))

(facts "About 'elevate-high'"
       (fact "simple example succeeds"
             (elevate-high [[0 0] [0 4] [2 4] [2 3] [1.5 3]] 1)
             => [[0 0] [0 (/ 16 5)] [(/ 6 5) 4] [2 (/ 18 5)] [(/ 19 10) 3] [1.5 3]]))


(facts "About 'reduce-degree'"
       (fact "succeeds"
             (matrix-round
              (reduce-degree [[0 0] [0 (/ 16 5)] [(/ 6 5) 4]
                              [2 (/ 18 5)] [(/ 19 10) 3] [1.5 3]]))
             => (matrix-round [[0 0] [0 4] [2 4] [2 3] [1.5 3]])))


;;
;; random tests
;;


;; TODO: find WORKING library for random testing;
;; tried:
;; - clojure.test.generative -> No such var: runner/run-vars
;; - clojure.test.check -> NullPointerException in 'bind'
;;
;; Test:
;; (cubic-bezier-coeffs m) <-> (bezier-polynomial m)
;; m <-> (reduce-degree (elevate m))
;;
