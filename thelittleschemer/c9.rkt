#lang racket
(require "./preset.rkt")
(require "./c4.rkt")
(require "./c7.rkt")

(define looking
  (lambda(a lat)
    (keep-looking a (pick 1 lat) lat)))

(define keep-looking
  (lambda(a sorn lat)
    (cond
      ((number? sorn)
       (keep-looking a (pick sorn lat)lat))
      (else(eq? sorn a)))))

(looking 'caviar '(6 2 grits caviar 5 7 3))
(looking 'caviar '(6 2 4 caviar 5 7 3))

(define eternity
  (lambda(x)
    (eternity x)))

(define shift
  (lambda(pair)
    (build(first(first pair))
          (build(second(first pair))
                (second pair)))))
(shift '((a b)(c d)))

(define align
  (lambda(pora)
    (cond
      ((atom? pora)pora)
      ((a-pair?(first pora))
       (align(shift pora)))
      (else(build(first pora)
                 (align(second pora)))))))