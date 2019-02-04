#lang racket
(define (next n)
  (if (= 2 n)
      3
      (+ n 2)))