#lang racket
(define (multi a b)
  (multi-iter a b 0))
(define (multi-iter a b c)
  (cond ((= b 0) c)
        ((even? b) (multi-iter (* a 2) (/ b 2) c))
        ((odd? b) (multi-iter a (- b 1) (+ c a)))))

(multi 2 3)