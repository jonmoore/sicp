#lang sicp
(#%require (only racket module+ define-namespace-anchor namespace-anchor->namespace) rackunit racket/trace)
(#%require "../utils/sicp-utils.rkt" "../utils/srfi-1.rkt")
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))


(define (make-from-mag-angle r a)
  (define (dispatch op)
    (cond ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          (else
           (error "Unknown op -- MAKE-FROM-MAG-ANGLE" op))))
  dispatch)

    
;; 2.76

;; adding types
;; hard in generic operations (construct operations that dispatch on type)
;; easy-ish in data-directed
;; easy in message-passing (construct types that dispatch on operation)

;; adding operations
;; easy in generic operations (construct operations that dispatch on type)
;; easy-ish in data-directed
;; hard in message-passing (construct types that dispatch on operation)
