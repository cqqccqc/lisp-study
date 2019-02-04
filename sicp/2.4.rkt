#lang racket
(define (square x)
  (* x x))
; real image
;(make-from-real-imag (real-part z) (imag-part z))
; magnitude angle
;(make-from-mag-ang (magnitude z) (angle z))

(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))
(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))
(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))
(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))
;x = r cosA
;y = r sinA
;r = sqrt(x^2 + y^2)
;A = arctan(y, x)
;Dikaer coordinate system
(define (real-part z) (car z))
(define (imag-part z) (cdr z))
(define (magnitude z)
  (sqrt (+ (square (real-part z)) (square (imag-part z)))))
(define (angle z)
  (atan (imag-part z) (real-part z)))
(define (make-from-real-imag x y) (cons x y))
(define (make-from-mag-ang r a)
  (cons (* r (cos a)) (* r (sin a))))

;polar coordinate system
(define (real-part2 z)
  (* (magnitude z) (cos (angle z))))
(define (imag-part2 z)
  (* (magnitude z) (sin (angle z))))
(define (magnitude2 z) (car z))
(define (angle2 z) (cdr z))
(define (make-from-real-imag2  x y)
  (cons (sqrt (+ (square x) (square y)))
        (atan y x)))
(define (make-from-mag-ang2 r a) (cons r a))