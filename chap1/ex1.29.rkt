#lang sicp
(#%require (only racket module+ format) rackunit)
(#%require "../utils/sicp-utils.rkt" "../utils/srfi-1.rkt")

(define (sum-iter term a next b)
  (define (iter a result)
    (if (>= a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

(define (simp f a b n)
  (define h (/ (- b a) n))
  (define (y k)
    (f (+ a (* k h))))
  (* (/ h 3.0)
     (sum-iter
      (lambda (k)
        (+ (y k) (* 4 (y (+ k 1))) (y (+ k 2))))
      0
      (lambda (k) (+ k 2))
      n)))
  
(module+ main
  (begin-example "1.30")
  (displn "  (sum-iter square 0 inc 3))")
  (sum-iter square 0 inc 3))
  
(define (product-iter term a next b)
  (define (iter a result)
    (if (>= a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define (accum-iter combiner null term a next b)
  (define (iter a result)
    (if (>= a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null))

(define (sum-accum term a next b)
  (accum-iter + 0 term a next b))

(define (product-accum term a next b)
  (accum-iter * 1 term a next b))

(define (pi n-max)
  (* 4.0
     (product-iter
      (lambda (n) (/ (* (inc n) (dec n)) (square n) 1.0))
      3
      (lambda (n) (+ n 2))
      n-max)))
     
