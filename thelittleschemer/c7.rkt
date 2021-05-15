#lang racket
(require "./preset.rkt")
(require "./c2.rkt")
(require "./c3.rkt")

(member? 2 '(1 2 3))

(define set?
  (lambda (lat)
    (cond
      ((null? lat)#t)
      (else
       (cond
         ((member?(car lat)(cdr lat)) #f)
         (else(set?(cdr lat))))))))

;(define makeset
;  (lambda(lat)
;    (cond
;      ((null? lat)(quote()))
;      ((member?(car lat)(cdr lat))
;       (makeset(cdr lat)))
;      (else(cons(car lat)
;                (makeset(cdr lat)))))))

(define makeset
  (lambda(lat)
    (cond ((null? lat)'())
          (else
           (cons(car lat)
                (makeset
                 (multirember(car lat)(cdr lat))))))))
(makeset '(1 1 2 3 3 4 5 6 5 2))

(define subset
  (lambda(set1 set2)
    (cond
      ((null? set1)#t)
      (else
       (and(member?(car set1)set2)
           (subset(cdr set1)set2))))))
(subset '(1 2) '(1 2 3))

(define eqset?
  (lambda(set1 set2)
    (and(subset? set1 set2)
        (subset? set2 set1))))
(eqset? '(1 2) '(1 2))

(define intersect?
  (lambda(set1 set2)
    (cond ((null? set1)#f)
          (else
           (or(member?(car set1)set2)
              (intersect?(cdr set1)set2))))))

(define intersect
  (lambda(set1 set2)
    (cond
      ((null? set1)'())
      ((member?(car set1)set2)
       (cons(car set1)
            (intersect(cdr set1)set2)))
      (else
       (intersect(cdr set1)set2)))))

(intersect '(1 2 3) '(2 3 4))

(define union
  (lambda(set1 set2)
    (cond
      ((null? set1)set2)
      ((member?(cdr set1)set2)
       (union(cdr set1)set2))
      (else(cons(car set1)
                (union(cdr set1)set2))))))
(union '(1 2 3) '(2 3 4))

(define xxx
  (lambda(set1 set2)
    (cond
      ((null? set1)'())
      ((member?(car set1)set2)
       (xxx(cdr set1)set2))
      (else(cons(car set1)
                (xxx(cdr set1)set2))))))


(define intersectall
  (lambda(l-set)
    (cond
      ((null?(cdr l-set))(car l-set))
      (else(intersect(car l-set)
                     (intersectall(cdr l-set)))))))