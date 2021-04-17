#lang racket
(require "./preset.rkt")
(require "./c4.rkt")

(define rember*
  (lambda (a l)
    (cond
      ((null? l)'())
      ((atom? (car l))
       (cond ((eq?(car l)a)
              (rember* a(cdr l)))
             (else(cons(car l)
                       (rember* a(cdr l))))))
      (else(cons(rember* a(car l))
                (rember* a(cdr l)))))))

(rember* "cup" '(("coffee")"cup"(("tea")"cup")("and"("hick"))))

(define insertR*
  (lambda (new old l)
    (cond
      ((null? l)'())
      ((atom? (car l))
       (cond ((eq?(car l)old)
              (cons old
                    (cons new
                          (insertR* new old
                                    (cdr l)))))
             (else (cons (car l)
                         (insertR* new old
                                   (cdr l))))))
      (else (cons (insertR* new old
                            (car l))
                  (insertR* new old
                            (cdr l)))))))

(define occur*
  (lambda (a l)
    (cond ((null? l) 0)
    ((atom? (car l))
     (cond ((eq? (car l) a)
            (add1(occur* a (cdr l))))
           (else (occur* a (cdr l)))))
    (else(o+ (occur* a (car l))
             (occur* a (cdr l)))))))

(occur* "banana" '(("banana")
          "split" (((("banana" "ice"))))
          ("banana")))

(define subset*
  (lambda (new old l)
    (cond ((null? l) '())
          ((atom? (car l))
           (cond ((eq? (car l) old)
                  (cons new (subset* new old (cdr l))))
                 (else (cons (car l)
                             (subset* new old (cdr l))))))
          (else (cons (subset* new old (car l))
                      (subset* new old (cdr l)))))))

(define insertL*
  (lambda (new old l)
    (cond
      ((null? l)'())
      ((atom?(car l))
       (cond ((eq?(car l)old)
              (cons new
                    (cons old
                          (insertL* new old
                                    (cdr l)))))
             (else(cons(car l)
                       (insertL* new old
                                 (cdr l))))))
      (else(cons(insertL* new old
                          (car l))
                (insertL* new old (cdr l)))))))