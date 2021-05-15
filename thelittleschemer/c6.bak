#lang racket
(require "./preset.rkt")
(require "./c4.rkt")

(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp)(number? aexp))
      (else
       (and(numbered?(car aexp))
           (numbered?(car(cdr(cdr aexp)))))))))

;(define value
; (lambda (nexp)
;   (cond
;    ((atom? nexp) nexp)
;    ((eq? (car (cdr nexp)) (quote o+))
;     (o+ (value (car nexp))
;        (value (car (cdr (cdr nexp))))))
;    ((eq? (car (cdr nexp)) (quote o*))
;     (o* (value (car nexp))
;        (value (car (cdr (cdr nexp))))))
;    (else
;     (oe (value (car nexp))
;        (value (car (cdr (cdr nexp)))))))))

(value '(1 o+ 2))
(value)

(define 1st-sub-exp
  (lambda(aexp)
    (car(cdr aexp))))

(define 2nd-sub-exp
  (lambda(aexp)
    (car(cdr(cdr aexp)))))

(define operator
  (lambda(aexp)
    (car aexp)))

(define value
 (lambda (nexp)
   (cond
    ((atom? nexp) nexp)
    ((eq? (operator nexp) (quote +))
     (o+ (value (1st-sub-exp nexp))
        (value (2nd-sub-exp nexp))))
    ((eq? (operator nexp) (quote x))
     (o* (value (1st-sub-exp nexp))
        (value (2nd-sub-exp nexp))))
    (else
     (oe (value (1st-sub-exp nexp))
        (value (2nd-sub-exp nexp)))))))


(define sero?
 (lambda (n)
   (null? n)))

(define edd1
 (lambda (n)
   (cons (quote ()) n)))

(define zub1
 (lambda (n)
   (cdr n)))

(define parenthesis+
 (lambda (n m)
   (cond
     ((sero? m) n)
     (else (edd1 (+ n (zub1 m)))))))