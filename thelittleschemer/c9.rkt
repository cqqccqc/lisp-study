#lang racket
(require "./c4.rkt")

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
