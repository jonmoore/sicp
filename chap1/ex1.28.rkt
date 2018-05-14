#lang sicp
(#%require (only racket module+ format) rackunit)
(#%require "../utils/sicp-utils.rkt" "../utils/srfi-1.rkt"
           "ex1.19.rkt" "ex1.21.rkt" "ex1.22.rkt" "ex1.24.rkt" "ex1.27.rkt")

(define (square-mod-signalling m)
  ;; squares x mod m, returning 0 to signal if a non-triv sqrt of 1
  ;; has been found, otherwise the normal result
  (lambda (x)
    (let ((sq (remainder (square x) m)))
      (if (and (not (= x 1))
               (not (= x (dec m)))
               (= sq 1))
          0
          sq))))

(define (expmod-signalling base exp m)
  ((make-fast-combiner-iter (square-mod-signalling m) (*-mod m) 1)
   base exp))

(define (mr-test n)
  (define (try-it a)
    (= 1 (expmod-signalling a (dec n) n)))
  (try-it (+ 1 (random (min 4294967087 (- n 1))))))

(define (mr-prime? n times)
  (cond ((= times 0) #t)
        ((mr-test n)
         (mr-prime? n (- times 1)))
        (else false)))

(define (make-mr-prime? times)
  (lambda (n)
    (mr-prime? n times)))

(module+ main
  (begin-example "1.28")

  (define (display-mr-results to-test)
    (displn
     (zip to-test
          (map (make-mr-prime? 100)
               to-test))))
  (display-mr-results '(2 3 5 7 11))
  (display-mr-results carmichael-numbers)
  (newline))

(module+ test
  (define (results-for power prime?)
    (map timing-base-value
         (take-n-primes-timed 3
                              (add 2)
                              prime?
                              (inc (expt 10
                                         power)))))
  
  (test-case
   "Checking results of prime? vs (make-mr-prime? 100)"
   (for-each
    (lambda (power)
      (with-check-info
       (('power power))
       (check-equal?
        (results-for power prime?)
        (results-for power (make-mr-prime? 100)))))
    (iota 6 3))))
