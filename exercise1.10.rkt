#lang racket
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))
                 )
              )
        )
  )

(A 1 10)
(A 2 4)
(A 3 3)

(define (pow x y)
  (if (= y 0)
      1
      (* x (pow x (- y 1)))
      )
  )

; 1. 1024 (2^n )
; 2. 65536 2^16
; 3. 65526 2^16
; 4. 2n
; 5. 2^n
; 6. 2^n^2