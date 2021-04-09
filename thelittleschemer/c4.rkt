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

(define tup+
  (lambda (tup1 tup2)
    (cond ((null? tup1) tup2)
          ((null? tup2) tup1)
          (else
           (cons (o+ (car tup1)(car tup2))
                 (tup+ (cdr tup1)(cdr tup2)))))))

(tup+ '(1 2 3) '(3 2 1))

(define o>
  (lambda (n m)
    (cond ((zero? m) #t)
          ((zero? n) #f)
          (else (o> (sub1 n)(sub1 m)))
          )))
(o> 1 2)
(o> 2 1)

(define o<
  (lambda (n m)
    (cond ((zero? m) #f)
          ((zero? n) #t)
          (else (o< (sub1 n)(sub1 m)))
          )))
(o< 1 2)
(o< 2 1)

(define o=
  (lambda (n m)
    (cond ((o> n m) #f)
          ((o< n m) #f)
          (else #t)
          )))
(o= 1 1)