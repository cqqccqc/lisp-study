#lang racket
;平面线段表示
;点坐标是实数的序对
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
;;;make-segment
(define (make-segment start-segment end-segment)
  (cons start-segment end-segment))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (midpoint-segment segment)
  (1))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define start-segment (make-rat 1 2))
(define end-segment (make-rat 1 2))
(print-point (make-segment start-segment end-segment))