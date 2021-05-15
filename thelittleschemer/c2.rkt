#lang racket
(require "./preset.rkt")

(define (lat0? l)
    (cond ((null? l) #t)
          (else (and (atom? (car l)) (lat? (cdr l)) ))
          )
  )

(define lat?
  (lambda (l)
    (cond ((null? l) #t)
          ((atom? (car l))(lat? (cdr l)))
          (else #f))))

(lat? '(1 2 3))
(lat? '('(1) 2 3))

(define member?
  (lambda (a lat)
    (cond ((null? lat) #f)
          (else (or (eq? a (car lat))(member? a (cdr lat))))
    )))
(member? 1 '(1 2 3))
(provide member?)