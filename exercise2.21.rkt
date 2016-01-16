#lang racket
(define (square-list items)
  (if (null? items)
      null
      (cons (expt (car items) 2) (square-list (cdr items)) )))

(square-list `(1 2 3))

(define (square-list-m items)
  (map (lambda (x) (expt x 2)) items))

(square-list-m `(1 2 3 ))