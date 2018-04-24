#lang sicp

;; Example for testing using rackunit.
;;
;; This example is a racket module written in SICP providing functions
;; that should be tested externally.

(define (double x)
  (* x 2))

(define (triple x)
  (* x 3))

(#%provide double triple)
