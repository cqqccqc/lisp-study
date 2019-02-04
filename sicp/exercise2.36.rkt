#lang racket
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(accumulate + 0 `(1 2 3))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init (car-n seqs))
            (accumulate-n op init (cdr-n seqs)))))

(define (car-n seq)
  (map car seq))
(define (cdr-n seq)
  (map cdr seq))