#lang racket
;; 树的递归,两个参数都要改变(proc (recurse (car tree)) (recurse (cdr tree)))
(define (fringe tree)
    (cond ((null? tree)                         ; 空树
            '())
          ((not (pair? tree))                   ; 叶子
            (list tree))
          (else
            (append (fringe (car tree))         ; 累积左子树所有元素
                    (fringe (cadr tree))))))    ; 累积右子树所有元素