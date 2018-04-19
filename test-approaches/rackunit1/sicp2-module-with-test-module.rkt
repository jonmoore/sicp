#lang sicp2

;; Example racket module with an embedded test module for testing using rackunit.
;;
;; "#lang sicp2" is essentially racket, "#lang racket" would also work.
;;
;; USAGE
;;
;; From the command-line
;;
;;  raco test <parent_dir>
;;
;; EXAMPLE OUTPUT
;;
;; > raco test rackunit
;; raco test: (submod "rackunit\\rackunit-example-sicp-module-with-embedded-automatically-run-tests.rkt" test)
;; --------------------
;; twice 2 is 4 for bad-double
;; FAILURE
;; name:       check
;; location:   rackunit-example-sicp-module-with-embedded-automatically-run-tests.rkt:54:3
;; params:     '(#<procedure:=> 44 4)
;; --------------------
;; 1/4 test failures

(define (double x)
  (* x 2))

(define (triple x)
  (* x 3))

(#%provide double triple)

;; We call the sub-module "test" so that "raco test" will
;; auto-discover the tests defined in it.

;; "#lang sicp2" provides everything defined in racket, so module+
;; works without "(require racket)".
(module+ test
  (require rackunit)

  ;; A simple set of test-cases.  The naming of test-cases explains
  ;; intent and is more user-friendly than running raw rackunit
  ;; checks.  Note that if we group our test-cases into a test-suite
  ;; then per the rackunit manual "Unlike a check or test case, a test
  ;; suite is not immediately run."  Instead we have to use use one of
  ;; the functions described in User Interfaces or Programmatically
  ;; Running Tests and Inspecting Results.

  (test-case
   "twice 2 is 4 for double"
   (check = (double 2) 4))

  (test-case
   "thrice 2 is 6 for triple"
   (check = (triple 2) 6)))
