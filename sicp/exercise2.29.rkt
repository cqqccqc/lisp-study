#lang racket

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cadr mobile))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cadr branch))

(define (total-weight mobile)
  (define (branch-weight branch)
    (if (pair? (branch-structure branch))
        (total-weight (branch-structure branch))
        (branch-structure branch))) ;;structure返回一个数字表示重量，或者一个活动体
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(branch-structure
 (left-branch
  (make-mobile
   (make-branch 2 (make-branch 2 4))
   (make-branch 4 5))))

