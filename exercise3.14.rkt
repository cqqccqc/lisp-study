#lang racket
(require rnrs/mutable-pairs-6)
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (mcdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

; reverse
(mystery (mcons 1 (mcons 2 (mcons 3 null))))