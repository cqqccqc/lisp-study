#lang racket

(define rember
  (lambda (a lat)
    (cond ((null? lat) '())
          (else (cond ((eq? (car lat) a)(cdr lat))
                      (else (cons (car lat)
                                  (rember a (cdr lat)))
                                  ))
                      )
                )
    ))

(rember 1 '(2 3 1 4 5))

(define firsts
  (lambda (l)
    (cond ((null? l) '())
          (else (cons (car (car l))(firsts (cdr l))))
          )
    ))

(firsts `((1 2 3)
          (4 5 6)
          (7 8 9)
          ))