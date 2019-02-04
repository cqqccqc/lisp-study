#lang racket
(define (make-monitored f)
  (let ((count 0))
    (lambda (x)
      (cond ((eq? x 'reset-count) (set! count 0))
            ((eq? x 'how-many-calls?) count)
            (else (begin (set! count (+ count 1))) (f x))))))

(define s (make-monitored sqrt))
(s 100)
(s 'how-many-calls?)