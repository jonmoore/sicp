#lang racket

(require rackunit)
(provide (all-defined-out))

(define eps-abs 1e-6)
(define eps-rel 1e-6)

(define (check-close-abs a b)
  (check-= a b eps-abs))

(define (check-=-rel a b eps)
  (check-= a b (* eps (abs (- b a)))))

(define (check-close-rel a b)
  (check-=-rel a b eps-rel))
