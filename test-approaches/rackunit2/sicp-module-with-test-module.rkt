#lang sicp
;; Example for testing using rackunit
;;
;; This example is a script written in SICP script with embedded
;; rackunit tests.
;;
;; From the command-line
;;
;;  raco test <parent_dir>

(define (double x)
  (* x 2))

(define (triple x)
  (* x 3))

;; we require racket to bring module+ into scope
(#%require racket rackunit)

(module+ test
  (test-case
   "twice 2 is 4"
   (check = (double 2) 4))

  (test-case
   "thrice 2 is 6"
   (check = (triple 2) 6)))
