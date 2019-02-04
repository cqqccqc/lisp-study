#lang racket
(define (last-pair items)
  (if (null? (cdr items))
      (car items)
      (last-pair (cdr items)))
  )

(define items (list 1 2 3 4 5))
(last-pair items)