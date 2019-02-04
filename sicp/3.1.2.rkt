;#lang racket

#lang planet neil/sicp
 
(define random-init 2)
(define (rand-update x)
  (remainder (+ (* 3 x) 4) 2))

(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))