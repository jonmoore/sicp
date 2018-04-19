#lang sicp

(#%require "utils.rkt")

(define (fast-expt-iter x n)
  (define (iter a b n)
    (if (= n 0)
        a
        (iter
         (if (even? n) a (* a b))
         (* b b)
         (floor (/ n 2)))))
  (iter 1 x n))

(fast-expt-iter 2 7)

(define (powers base to)
  (map (lambda (power)
         (fast-expt-iter base power))
       (from-to 0 to)))

(powers 3 10)