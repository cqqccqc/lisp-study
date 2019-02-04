#lang racket
(require "exercise2.40.rkt")
(unique-pairs 6)

(define (unique-triples n)
    (flatmap (lambda (i)
                 (map (lambda (j)                   ; cons 起 i 元素和二元组 j ,组成三元组
                          (cons i j))
                      (unique-pairs (- i 1))))      ; 生成不大于 i 的所有相异整数二元组
             (enumerate-interval 1 n)))             ; 生成 1 至 n 的所有整数，作为 i

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))
(define (triple-sum-equal-to? sum triple)
    (= sum
       (+ (car triple)
          (cadr triple)
          (caddr triple))))

(define (remove-triples-not-equal-to sum triple)
    (filter (lambda (current-triple)
                (triple-sum-equal-to? sum current-triple))
            triple))