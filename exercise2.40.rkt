#lang racket
;#lang planet neil/sicp
(provide unique-pairs)
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))

(enumerate-interval 1 5)
(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map
              (lambda (j)
                (list i j))
              (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))
