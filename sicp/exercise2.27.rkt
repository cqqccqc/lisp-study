#lang racket
;;树的递归，两个参数都要改变(proc (recurse (car tree)) (recurse (cdr tree)))
(define (deep-reverse tree)
    (cond ((null? tree)         ; 空树
            '())
          ((not (pair? tree))   ; 叶子
            tree)
          (else
            (reverse (list (deep-reverse (car tree))            ; 递归地逆序左右子树
                           (deep-reverse (cadr tree)))))))