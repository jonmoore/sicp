#lang sicp

(#%require (only racket module+) rackunit)
(#%require "../utils/sicp-utils.rkt" "../utils/srfi-1.rkt")

(define (fast-expt-iter x n)
  (define (iter a b n)
    (if (= n 0)
        a
        (iter (if (even? n) a (* a b))
              (* b b)
              (floor (/ n 2)))))
  (iter 1 x n))

(define (powers base to)
  (map (lambda (power)
         (fast-expt-iter base power))
       (iota (inc to))))

(module+ main
  (begin-example "1.16")
  (displn
   (fast-expt-iter 2 7))

  (displn
   (powers 3 10)))

(module+ test
  (test-case
   "Check fast-expt-iter vs expt"
   (for-each
    (lambda (base)
      (for-each
       (lambda (power)
         (check-true
          (close-in-rel-error?
           (expt base power)
           (fast-expt-iter base power)
           1e-4)))
       (iota 5)))
    '(0.01 0.1 1.0 10.0))))

