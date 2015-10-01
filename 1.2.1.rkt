#lang racket
; factorial
; n! = n*(n-1)*(n-2)...3*2*1

; recuse 
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))
      )
  )

(factorial 3)
(factorial 4)

; iterate
(define (factorial-i n)
  (define (fact-iter product counter max-count)
    (if (> counter max-count)
        product
        (fact-iter (* counter product)
                   (+ counter 1)
                   max-count)
        )
    )
  (fact-iter 1 1 n)
  )
(factorial-i 4)