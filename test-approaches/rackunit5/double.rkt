#lang sicp

;; racket is used to provide module+, which is how we are using
;; rackunit.
(#%require racket rackunit)

;; The #%require above brings racket's definitions in scope.  To avoid
;; this we can use a sub-module with language sicp.  Alternatively we
;; could put the definitions in their own file, with separate tests.
(module inner sicp
  (define baz 1)
  (define (double x)
    (* x 2))
  ;; (printf "hello\n") ;; Racket's printf is unbound here 
  (#%provide baz double))

(require 'inner)

(module+ test
   (test-case
   "check using definition "
   (check = baz 1))

  (test-case
   "check twice 2 is 4"
   (check = (double 2) 4)))

(module+ main
  (define (foo) 42)
  (define bar 42)
  (provide bar))

(provide double)
