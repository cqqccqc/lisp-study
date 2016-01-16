#lang racket
(define (same-parity a . b)
  (if (even? a)
      (2)
      (1))
  )

(define (filter-even numbers)
  (define (iter remained-items result)
    (if (null? remained-items)
        null
        (let ((number (car remained-items)))
          (if (even? number)
              (iter (cdr remained-items) (cons result number))
              (iter (cdr remained-items) result)
          )
        )))
  (iter numbers `())
  )

(filter-even '(1 2 3))