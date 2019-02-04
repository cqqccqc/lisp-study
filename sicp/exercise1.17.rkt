#lang racket
(define (* a b)
  (cond ((= b 0) 0)
        ((even? b) (* (double a) (halve b)))
        ((odd? b) (+ a (* a (- b 1))) )))
(define (double n)
  (+ n n))
(define (halve n)
  (/ n 2))

(* 2 3)