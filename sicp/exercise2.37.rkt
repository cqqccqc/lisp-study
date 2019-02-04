#lang racket

; accumulate
(define (accumulate op init sequence)
  (if (null? sequence)
      init
      (op (car sequence)
          (accumulate op init (cdr sequence)))))
; accumulate-n
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

; dot-product v w
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

; matrix-*-vector m v
(define (matrix-*-vector m v)
  (map (lambda (col) (dot-product col v))
       m))

; transpose
(define (transpose mat)
  (accumulate-n cons `() mat))
(transpose (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
; matrix-*-matrix m n
(define (matrix-*-matrix m n)
  (let ((cols-of-n (transpose n)))
    (map (lambda (cols-of-m) (matrix-*-vector cols-of-n cols-of-m)) m)))
