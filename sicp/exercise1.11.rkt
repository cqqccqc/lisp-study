#lang racket
; recurse

(define (f1 n)
  (cond ((< n 3) n)
        (else (+ (f1 (- n 1))
                 (* 2 (f1 (- n 2)))
                 (* 3 (f1 (- n 3)))
                 )
              )
        )
  )

(f1 0)
(f1 1)
(f1 2)
(f1 3)


; iterate
(define (f2 n)
  (f-iter 0 1 2 n))
(define (f-iter a b c n)
  (if (= n 0)
      a
      (f-iter b c (+ (* 3 a) (* 2 b) c)  (- n 1))))
(f2 0)
(f2 1)
(f2 2)
(f2 3)