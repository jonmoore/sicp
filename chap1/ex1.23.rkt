#lang sicp
(#%require (only racket module+ format) rackunit racket/trace)
(#%require "../utils/sicp-utils.rkt" "../utils/srfi-1.rkt"
           "ex1.21.rkt" "ex1.22.rkt")

(define (prime?2 n)
  (define (next x)
    (if (= x 2) 3 (+ x 2)))
  (= n (find-divisor n 2 next)))

(define (compare-prime-algos prime? prime?2 range)
  (for-each
   (lambda (power)
     (let* ((n (inc (expt 10 power)))
            (time1 ((time-to-test-primes 3 prime? (add 2)) n))
            (time2 ((time-to-test-primes 3 prime?2 (add 2)) n)))
       (displn (list
                power
                time1
                time2
                (/ time2 time1)))))
   range))

(module+ main
  (begin-example "1.23")
  (displn "Comparing prime? prime?2")
  (displn "(power time1 time2 (/ time2 time1))")
  (compare-prime-algos prime? prime?2 (iota 6 10)))

;; Answer generally between 50% and 60% of time with test that always
;; increments the divisor for large numbers.
