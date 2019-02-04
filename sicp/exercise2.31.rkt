#lang racket
;;; define square
(define (square x) (* x x))
;;; one solution
(define (tree-map-new f tree)
    (cond ((null? tree)                         ; 空树
            '())
          ((not (pair? tree))                   ; 叶子节点
            (f tree))
          (else
            (cons (tree-map-new f (car tree))       ; 递归处理左右子树
                  (tree-map-new f (cdr tree))))))

;;; define tree-map second solution
(define (tree-map f tree)
    (map (lambda (sub-tree)
             (if (pair? sub-tree)
                 (tree-map f sub-tree)  ; 处理子树
                 (f sub-tree)))         ; 处理节点
         tree))

(define (square-tree tree)
  (tree-map square tree))

(tree-map square (list (list 1 2) (list 3 4)))