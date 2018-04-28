#lang sicp

(#%require (only racket module+) rackunit)

(#%provide double)
(define (double x)
  (* x 2))

(module+ test
  (test-case
   "check twice 2 is 4"
   (check = (double 2) 4)))

(module+ main
  (define (foo) 42)
  (define bar 42)
  (#%provide bar))
