#lang racket
(require "./preset.rkt")

(atom? 'atom)

(define s '(("help") "this"))
(define l '("is" "very" ("hard") "to" "learn"))
(cons s l)