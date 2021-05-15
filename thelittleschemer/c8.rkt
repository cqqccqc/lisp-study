#lang racket
(require "./preset.rkt")
(require "./c4.rkt")
(require "./c6.rkt")

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

;(define insertL(insert-g seqL))
;(define insertR(insert-g seqR))

(define insertL
  (insert-g
   (lambda (new old l)
     (cons new (cons old l)))))

(define insertR
  (insert-g
   (lambda(new old l)
     (cons old(cons new l)))))

;(define subst
;  (lambda(new old l)
;    (cond
;      ((null? l)'())
;      ((eq?(car l)old)
;       (cons new(cdr l)))
;      (else(cons(car l)
;                (subst new old(cdr l)))))))
(define seqS
  (lambda(new old l)
    (cons new l)))
(define subst(insert-g seqS))

(define yyy
  (lambda(a l)
         ((insert-g seqrem)#f a l)))
(define seqrem
  (lambda(new old l)l))

(define atom-to-function
  (lambda(x)
    (cond
      ((eq? x (quote +))o+)
      ((eq? x (quote *))o*)
      (else oe))))
((atom-to-function +) 2 5)

(define value
  (lambda(nexp)
    (cond
      ((atom? nexp)nexp)
      (else
       ((atom-to-function(operator nexp))
        (value(1st-sub-exp nexp))
        (value(2nd-sub-exp nexp)))))))
(eq? + +)
(eq? (operator '(+ 2 3)) +)
(value '(+ 2 3))

(define multirember-f
  (lambda (test?)
  (lambda(a lat)
    (cond
      ((null? lat)'())
      ((test? a (car lat))
       ((multirember-f test?)a(cdr lat)))
      (else(cons(car lat)
                ((multirember-f test?)a
                                      (cdr lat))))))))