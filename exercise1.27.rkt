#lang racket
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (square x)
  (* x x))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))
;;;561 1105 1729 2465 2821 6601
(fast-prime? 561 10)
(fast-prime? 561 1105)
(fast-prime? 561 1729)
(fast-prime? 561 2465)
(fast-prime? 561 2821)
(fast-prime? 561 6601)