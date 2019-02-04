#lang racket
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))
; 3
(define z1 '(1 2 3))
(count-pairs z1)

; 4
(define z2 '(1 '(2 3)))
(count-pairs z2)

; 7
;(define z3 )
;(count-pairs z3)
