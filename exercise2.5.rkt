#lang racket

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))
;;; car = 2的个数
(define (car z)
    (if (= 0 (remainder z 2))
        (+ 1 (car (/ z 2)))
        0))
;;; cdr = 3的个数
(define (cdr z)
    (if (= 0 (remainder z 3))
        (+ 1 (cdr (/ z 3)))
        0))