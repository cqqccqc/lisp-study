#lang racket
(define (square-tree tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))
(square-tree `(1 2 3))

(define (map-square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (map-square-tree sub-tree)
             (* sub-tree sub-tree)))
       tree))

(map-square-tree `(1 2 4))