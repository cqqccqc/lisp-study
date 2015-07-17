#lang racket
(define (sum_of_max_two x y z)
  (if (> (if (> (+ x y) (+ x z)) (+ x y) (+ x z)) (+ y z))
   (if (> (+ x y) (+ x z)) (+ x y) (+ x z)) (+ y z)))
(sum_of_max_two 1 2 3)