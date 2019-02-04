#lang racket
(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

(define (append! x y)
  (define (last-pair x)
    (if (null? (cdr x))
        (car x)
        (last-pair (cdr x))))
  (cons (last-pair x) y))

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
z
(cdr x)

(define w (append! x y))
w
(cdr x)
