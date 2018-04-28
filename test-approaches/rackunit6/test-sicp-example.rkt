#lang sicp

(#%require "sicp-example.rkt")
(#%require (only racket module+) rackunit)
;; (#%require  racket)  ;; not this - see the explanation below

(module+ test
(test-case
 "testing against a sicp list with '(0 1)"
 (check-equal?
  (sicp-list-example)
  (list 0 1))))

(module+ test
(test-case
 "testing against a sicp list with (list 0 1)"
 (check-equal?
  (sicp-list-example)
  (list 0 1))))

;; explanation of "(only racket module+)"

;; If we #%require "racket" rather than "(only racket module+)" above
;; this will change the interpretation of '(0 1) to use racket,
;; resulting in an error as below.  The same error report is seen when
;; testing against (list 0 1) rather than '(0 1).

;;
;; testing against a sicp list with '(0 1)
;; FAILURE
;; name:       check-equal?
;; location:   test-sicp-example.rkt:7:1
;; actual:     (mcons 0 (mcons 1 '()))
;; expected:   '(0 1)
