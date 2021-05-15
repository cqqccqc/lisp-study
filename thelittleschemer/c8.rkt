#lang racket
(require "./preset.rkt")
(require "./c5.rkt")

;(define rember-f
;  (lambda(test? a l)
;    (cond
;      ((null? l)'())
;      (else
;       (cond
;         ((test?(car l)a)(cdr l))
;         (else(cons(car l)
;                   (rember-f test? a (cdr l)))))))))

;(define rember-f
;  (lambda(test? a l)
;    (cond
;      ((null? l)'())
;      ((test? (car l)a)(cdr l))
;      (else(cons(car l)
;                (rember-f test? a (cdr l)))))))

(define eq?-c
  (lambda(a)
    (lambda(x)
      (eq? x a))))

(define k "salad")
(define eq?-salad (eq?-c k))
(define y "salad")
(eq?-salad "salad")


(define rember-f
  (lambda(test?)
    (lambda(a l)
      (cond
        ((null? l)'())
        ((test?(car l)a)(cdr l))
        (else(cons(car l)
                  (rember-f test?) a (cdr l)))))))