#lang racket
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (reduce n d)
  (let ((g (gcd n d)))
    (cons (/ (abs n) g) (/ d g))))

(define numer car)
(define denom cdr)

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (make-rat n d)
  (cond ((and (< n 0) (< d 0)) (reduce (- n) (- d)))
        ((and (< n 0) (> d 0)) (reduce n (- d)))
        ((and (> n 0) (< d 0)) (reduce (- n) d ))
        (else (reduce n d))
        )
  )

(print-rat (make-rat 2 4))
(print-rat (make-rat -2 4))
(print-rat (make-rat 2 -4))
(print-rat (make-rat -2 -4))