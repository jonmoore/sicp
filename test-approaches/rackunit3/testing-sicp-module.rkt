#lang sicp
;; Example for testing using rackunit
;;
;; This example shows how to test functions in an externally defined
;; racket module, "sicp-module.rkt".

(#%require racket rackunit "sicp-module.rkt")

(module+ test
  (test-case
   "twice 2 is 4"
   (check = (double 2) 4))

  (test-case
   "thrice 2 is 6"
   (check = (triple 2) 6)))
