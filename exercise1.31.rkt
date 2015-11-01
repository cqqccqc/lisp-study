#lang racket
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b)
         )
      )
  )
(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (+ a 1)
              (* (term a) result)))
    )
  (iter a 1)
  )

(define (factorial n)
  (product (lambda (x) x)
           1
           (lambda (i) (+ i 1))
           n
           )
  )

;; PI
(define (dec x)
  (- x 1))
(define (inc x)
  (+ x 1))
(define (f x)
  (* (/ (dec x) x)
     (/ (inc x) x)))
(define (plus-2 x)
  (+ x 2.0))
(define pi
  (* 4 (product f 3 plus-2 100000)))
pi