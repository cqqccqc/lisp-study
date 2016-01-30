#lang racket


(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


(define (map p sequence)
  (accumulate (lambda (x y)
                (cons (p x) y)) null sequence))

(map sqrt `(1 2 3 4))

(define (append seq1 seq2)
  (accumulate cons (1) (2) ))


(define (length sequence)
  (accumulate (1) 0 sequence))