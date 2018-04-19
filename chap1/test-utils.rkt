#lang sicp

(#%require "utils.rkt")

(from-to 5 10)

(define double
  (lambda (x)
    (* 2 x)))

(double 3)

(from-to-by 0 3 0.5)

(log10 100)
