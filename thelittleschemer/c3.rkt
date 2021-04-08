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


(define insertR
  (lambda (new old lat)
    (cond ((null? lat) '())
          (else
           (cond ((eq? (car lat) old) (cons old (cons new (cdr lat))))
                      (else (cons (car lat)(insertR new old (cdr lat)))))))
    ))

(insertR 10 3 '(1 2 3 4 5 6))

(define insertL
  (lambda (new old lat)
    (cond ((null? lat) '())
          (else (cond ((eq? (car lat) old)(cons new lat))
                      (else (cons (car lat)(insertL new old (cdr lat)))))))
    ))

(define subset
  (lambda (new old lat)
    (cond ((null? lat) '())
          (else (cond ((eq? (car lat) old) (cons new (cdr lat)))
                      (else (cons (car lat)(subset new old (cdr lat))))))
          )
    ))

(subset 10 3 '(1 2 3 4 5 6))

(define subset2
  (lambda (new o1 o2 lat)
  (cond ((null? lat) '())
        (else (cond ((or (eq? (car lat) o1)(eq? (car lat) o2))(cons new (cdr lat)))
                    (else (cons (car lat)(subset2 new o1 o2 (cdr lat))))
              )))
    ))

(subset2 10 5 6 '(1 2 3 4 5 6))