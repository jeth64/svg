(ns svg.bezier-test
  (:require [clojure.test :refer :all]
            [midje.sweet :refer [facts fact]]
            [svg.bezier :refer :all]))

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
