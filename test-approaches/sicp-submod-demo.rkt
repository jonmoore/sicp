#lang sicp
(#%require (only racket module module+))

;; Define modules using "module sicp".  Using "module+ inner" doesn't work with the
;; following module-level #%require although it does work when used from inner2 or main
;; below.
(module inner sicp
  (#%provide displn displn-inner)
  (define (displn text)
    (display text (current-output-port))
    (newline))
  (define (displn-inner)
    (displn "inner")))

;; Reference sub-modules at module-level using #%require, optionally with prefix.

;; These two are equivalent, and are commented out to make clear where displn and
;; displn-inner are referenced from below.

;; (#%require 'inner)
;; (#%require (submod "." inner))

(#%require (prefix inner- 'inner))
;; equivalently (#%require (prefix inner- (submod "." inner)))

(define (displn-module-level)
  (inner-displn "module-level"))

;; Reference sibling modules using (submod ".." ...) to start from the right place
(module inner2 sicp
  (#%require (submod ".." inner))
  (#%provide display-inner2)
  (define (display-inner2)
    (displn "inner2")))

(module+ main
  (#%require (submod ".." inner))
  (displn-inner)
  (displn-module-level)
  (#%require (submod ".." inner2))
  (display-inner2))
