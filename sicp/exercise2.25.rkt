#lang racket
(define list1 `(1 3 (5 7) 9) )
(define list2 `((7)) )
(define list3 `(1 (2 (3 (4 (5 (6 7)))))) )

(car (cdr (car (cdr (cdr list1)))))

(car (car list2))

(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr list3))))))))))))