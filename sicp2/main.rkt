#lang racket/base

;; Sets up this directory to define a language extension of racket-base

(require racket)

;; 1. Export everything imported from racket
;; See https://docs.racket-lang.org/reference/require.html#%28form._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._all-from-out%29%29
(provide (all-from-out racket))

;; 2. Define and export some helpers.
(define runtime current-process-milliseconds)

;; We can define as many helpers as we want as below
;; (define (square x)
;;   (* x x))

;; Export everything defined here - see
;; See https://docs.racket-lang.org/reference/require.html#%28form._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._all-defined-out%29%29
(provide (all-defined-out))
