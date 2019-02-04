#lang racket
(define (make-accumulator initial)
  (let ((accmulate initial))
    (lambda (sum) (begin (set! accmulate (+ accmulate sum)) accmulate))))

(define A (make-accumulator 5))
(A 10)