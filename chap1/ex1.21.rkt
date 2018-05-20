#lang sicp
(#%require (only racket module+ format) rackunit racket/trace)
(#%require "../utils/sicp-utils.rkt" "../utils/srfi-1.rkt")

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex 1.21
(define (divides? a b)
  (= (remainder b a) 0))

(define (find-divisor n test-divisor next)
  (cond ((> (square test-divisor) n)
         n)
        ((divides? test-divisor n)
         test-divisor)
        (else (find-divisor n (next test-divisor) next))))

(define (smallest-divisor n)
  (find-divisor n 2 inc))

(define (prime? n)
  (= n (smallest-divisor n)))

(module+ main
  (begin-example "1.21")
  
  (let ((numbers '(199 1999 19999)))
    (display (zip numbers (map smallest-divisor numbers)))))

(module+ test
  (check-equal?
   (map smallest-divisor '(199 1999 19999))
   '(199 1999 7)))

(#%provide (all-defined))
