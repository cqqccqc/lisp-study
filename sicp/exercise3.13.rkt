#lang racket
(require rnrs/base-6)
(require rnrs/mutable-pairs-6)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(last-pair '(1 2 3 4 5 6 7))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))
z

;(last-pair z) 