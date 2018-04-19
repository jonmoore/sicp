#lang racket/load

;; Example for testing using rackunit.  We want to test a script
;; written in lang SICP with external rackunit tests.  Currently only
;; works with scripts written in bare racket 
;;
;; From the command-line
;;
;;  raco test <file-name>

(load "sicp-script.rkt")

(#%require rackunit rackunit/text-ui)

(run-tests
 (test-suite "my suite"
    (test-case
     "twice 2 is 4"
     (check = (double 2) 4)))) 
