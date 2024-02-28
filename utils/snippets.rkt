#lang sicp

;; standard header

(#%require (only racket module+ define-namespace-anchor namespace-anchor->namespace) rackunit)
(#%require "../utils/sicp-utils.rkt" "../utils/srfi-1.rkt")

(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

;; using displn-eval and main

(module+ main
  (displn-eval 
   '(cons "the answer" (+ 21 21))
   ns))

(define (add x y)
  (+ x y))

;; using test, begin-example, test-case and with-check-info
(module+ test
  (begin-example "x.yz, ")
  (test-case
   "foo"
   (for-each
    (lambda (x)
      (with-check-info (('x x))
                       (check-equal? (add x x) (* 2 x))))
    (iota 6 3))))
