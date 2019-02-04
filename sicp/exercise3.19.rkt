#lang racket
; 使用两个指针，一个一次走一步，一个一次走两步，两者相遇则有环，否则走到底就表示无环
(define (loop? lst)
    (define (iter x y)
        (let ((x-walk (list-walk 1 x))
              (y-walk (list-walk 2 y)))
            (cond ((or (null? x-walk) (null? y-walk))
                    #f)
                  ((eq? x-walk y-walk)
                    #t)
                  (else
                    (iter x-walk y-walk)))))
    (iter lst lst))

(define (list-walk step lst)
    (cond ((null? lst)
            '())
          ((= step 0)
            lst)
          (else
            (list-walk (- step 1)
                       (cdr lst)))))