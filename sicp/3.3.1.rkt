#lang racket
(require rnrs/base-6)
(require rnrs/mutable-pairs-6)

(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)

(define x (list 'a 'b))
(define z1 (cons x x))
(define z2 (cons (list 'a 'b) (list 'a 'b)))

(eq? (car z1)(cdr z1))
(eq? (car z2)(cdr z2))