#lang sicp
(#%require (only racket module+ format) rackunit)
(#%require "../utils/sicp-utils.rkt" "../utils/srfi-1.rkt"
           "ex1.19.rkt"
           "ex1.22.rkt"
           )
;; (#%require (only racket compile-enforce-module-constants))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder 
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder 
          (* base (expmod base (- exp 1) m))
          m))))

(define (square-mod m)
  (lambda (x)
    (remainder (square x) m)))

(#%provide *-mod)
(define (*-mod m)
  (lambda (a b)
    (remainder (* a b) m)))

(#%provide expmod-)
(define (expmod- base exp m)
  ((make-fast-combiner-iter (square-mod m) (*-mod m) 1)
   base exp))

(module+ test
  (test-case
   "Check expmod and expmod- are equivalent"
   (for-each
    (lambda (m)
      (for-each
       (lambda (a)
         (for-each
          (lambda (n)
            (with-check-info
             (('m m) ('a a) ('n n))
             (check-equal? (expmod a n m) (expmod- a n m))))
          (iota (dec m) 1)))
       (iota (dec m) 1)))
    (iota 8 1))))


(define (fermat-test n)
  ;; Fermat test.  We don't pick from the whole range of numbers
  ;; because the built-in integer random generator doesn't handle
  ;; values outside a 32-bit range.
  (define (try-it a)
    (= (expmod- a n n) a))
  (try-it (+ 1 (random (min 4294967087 (- n 1))))))

;; The following procedure runs the test a given number of times, as
;; specified by a parameter. Its value is true if the test succeeds
;; every time, and false otherwise.

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) 
         (fast-prime? n (- times 1)))
        (else false)))

(define (make-fast-prime? times)
  (lambda (n)
    (fast-prime? n times)))

(module+ main
  (begin-example "1.24")

  (displn "First 3 primes >= 10^n+1 for n in (3,4,5,6)")
  (for-each
   (lambda (power)
     (displn
      (map timing-base-value
           (take-n-primes-timed 3
                                (add 2)
                                (make-fast-prime? 100)
                                (inc (expt 10 power))))))
   (iota 4 3))
  (newline)
  
  (displn "Sample timings from time-to-test-primes")
  (for-each
   (lambda (power)
     (displn
      (list power
            ((time-to-test-primes 10
                                  (make-fast-prime? 100)
                                  (add 2))
             (inc (expt 10 power))))))
   (iota 20 3))
  (newline))

;; ;; 1.25 Will have issues because working with huge numbers

;; ;; 1.26 Time obeys T(n) ~= 2*T(n/2), so T~O(n)

