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

(define length*
  (lambda(pora)
    (cond
      ((atom? pora)1)
      (else
       (o+(length*(first pora))
         (length*(second pora)))))))

(define weight*
  (lambda(pora)
    (cond
      ((atom? pora)1)
      (else
       (o+(o*(weight*(first pora))2)
          (weight*(second pora)))))))
(weight* '((a b) c))

(define shuffle
  (lambda(pora)
    (cond
      ((atom? pora)pora)
      ((a-pair?(first pora))
       (shuffle(revpair pora)))
      (else(build(first pora)
                 (shuffle(second pora)))))))
(shuffle '(a(b c)))
(shuffle '((a b)(c d)))

(lambda(length)
  (lambda(l)
    (cond
      ((null? l)0)
      (else(add1(length(cdr l)))))))

(define Y
  (lambda(le)
    ((lambda(f)(f f))
     (lambda(f)
       (le(lambda(x)((f f)x)))))))