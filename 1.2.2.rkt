#lang racket
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+
               (fib (- n 1))
               (fib (- n 2))
               )
              )
        )
  )

(define (fib2 n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))
        )
    )
  (fib-iter 1 0 n)
  )
(fib2 1)
(fib2 2)
(fib2 3)
(fib2 4)
(fib2 5)
(fib2 6)

; change coin
; amount零钱还n种类硬币
; 换零钱的方法的总数等于
; 不使用第一种面值的所有还发 + 使用第一种面值的所有还发
(define (change-coin amount)
  
  (define (first-count n)
    (cond ((= n 1) 1)
          ((= n 2) 5)
          ((= n 3) 10)
          ((= n 4) 25)
          ((= n 5) 50)
          )
    )
  (define (cc amount n)
      (cond ((= amount 0) 1)
            ((or (< amount 0) (= n 0)) 0)
            (else (+ (cc amount (- n 1))
                     (cc (- amount (first-count n)) n)
                     )
                  )
            )
      )
  (cc amount 5)
  )
(change-coin 100)
  