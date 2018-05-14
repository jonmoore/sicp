#lang sicp
(#%require (only racket module+ format) rackunit)
(#%require "../utils/sicp-utils.rkt" "../utils/srfi-1.rkt"
           "ex1.24.rkt"
           )
;; (#%require (only racket compile-enforce-module-constants))

(#%provide carmichael-numbers)
(define carmichael-numbers '(561 1105 1729 2465 2821 6601))

(define (full-fermat-test? n)
  (define (try-it a)
    (= (expmod- a n n) a))
  (every try-it (iota (dec n) 1)))

(module+ main
  (begin-example "1.27")
  (displn "applying full-fermat-test? to Carmichael numbers")
  (displn (zip carmichael-numbers
               (map full-fermat-test? carmichael-numbers))))

(module+ test
  (test-case
   "Check that Carmichael numbers fool full-fermat-test?"
   (for-each
    (lambda (carmichael-number)
      (check-true (full-fermat-test? carmichael-number)))
    carmichael-numbers)))
