#lang racket
(define (sqrt x)
  (sqrt-iter 1.0 x))
(define (sqrt-iter guess x)
  (if (good-enough?2 guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (good-enough?2 guess x)
  (< (/ (improve guess x) guess) 1))

(define (improve guess x)
  (average guess (/ x guess)))
(define (square x)
  (* x x))
(define (average x y)
  (/ (+ x y) 2))

(sqrt 2)