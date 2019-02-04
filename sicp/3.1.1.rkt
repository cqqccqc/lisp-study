#lang racket
(define balance 100)
(define (withdraw amout)
  (if (>= balance amout)
      (begin (set! balance (- balance amout))
             balance)
      "Insufficient funds"))

(define new-withdraw
  (let ((balance 100))
    (lambda (amout)
      (if (>= balance amout)
          (begin (set! balance (- balance amout))
                 balance)
          "Insufficient funds"))))