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

(define insertL-f
  (lambda(test?)
    (lambda(new old l)
      (cond
        ((null? l)'())
        ((test?(car l)old)
         (cons new (cons old(cdr l))))
        (else(cons(car l)
                  ((insertL-f test?)new old
                                    (cdr l))))))))

(define insertR-f
  (lambda(test?)
    (lambda(new old l)
      (cond
        ((null? l)'())
        ((test?(car l)old)
         (cons old(cons new (cdr l))))
        (else(cons(car l)
                  ((insertR-f test?)new old
                                    (cdr l))))))))

(define seqL
  (lambda(new old l)
    (cons new(cons old l))))
(define seqR
  (lambda(new old l)
    (cons old(cons new l))))

(define insert-g
  (lambda(seq)
    (lambda(new old l)
      (cond ((null? l)'())
            ((eq?(car l)old)
             (seq new old (cdr l)))
            (else(cons(car l)
                      ((insert-g seq)new old
                                     (cdr l))))))))