#lang racket
(define (for-each p lst)
    (cond ((not (null? lst))
            (p (car lst))
            (for-each p (cdr lst)))
          )
  )

(define (new-for-each p lst)
  (if (null? lst)
      null
      ((p (car lst))
            (for-each p (cdr lst))))
  )
(for-each sqrt `(1 2 3 ))