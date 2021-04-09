#lang racket

(define add1
  (lambda (n) (+ n 1)))

(add1 10)

(define sub1
  (lambda (n) (- n 1)))

(define o+
  (lambda (n m)
    (cond ((zero? m) n)
          (else (add1 (o+ n (sub1 m))))
          )
    ))
(o+ 3 4)

(define o-
  (lambda (n m)
    (cond ((zero? m) n)
          (else (sub1 (o- n (sub1 m)))))))
(o- 3 2)

(define addtup
  (lambda (tup)
    (cond ((null? tup) 0)
          (else (o+ (car tup)(addtup (cdr tup)))))))

(addtup '(1 2 3 4 5))

(define o*
  (lambda (n m)
    (cond ((zero? m) 0)
          (else (o+ n (o* n (sub1 m)))))))
(o* 10 20)

