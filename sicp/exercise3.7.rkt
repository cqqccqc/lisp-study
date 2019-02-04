#lang racket
(define (in-pwds? tpwd tpwds)
    (if (null? tpwds)
        'f
        (if (eq? (car tpwds) tpwd)
            't
            (in-pwds? tpwds (cdr tpwds)))))

(define (make-account balance secret-password)
  (define pwds '(secret-password))
  (define (in-pwds? tpwd tpwds)
    (if (null? tpwds)
        'f
        (if (eq? (car tpwds) tpwd)
            't
            (in-pwds? tpwds (cdr tpwds)))))
  (define (add-pwd new-pwd)
    (set! pwds (cons new-pwd pwds)))
  (define (withdraw amount)  
    ((if (>= balance amount)
         (begin (set! balance (- balance amount))
                balance)
         "Insufficient funds")))
  (define (deposit amount)    
    ((set! balance (+ balance amount))
     balance))
  (define (dispatch m pwd)
    (if (in-pwds? pwd pwds)
        (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          ((eq? m 'add-pwd) add-pwd)
          (else (error "Unkown request -- MAKE-ACCOUNT" m)))
        "Incorrect password"))
  dispatch)

(define (make-joint account old-pwd new-pwd)
  ((account 'add-pwd old-pwd) new-pwd)
  account)

(define acc (make-account 100 'secret-password))
(define new-acc (make-joint acc 'secret-password 'ss))
(new-acc 'withdraw 'ss)


;(acc 'withdraw 's)