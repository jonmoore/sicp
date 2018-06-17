#lang sicp
(#%require (only racket module+ define-namespace-anchor namespace-anchor->namespace) rackunit)
(#%require "../utils/sicp-utils.rkt" "../utils/srfi-1.rkt")
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items))
            (square-list (cdr items)))))

(define (square-list-2 items)
  (map square items))

(module+ main
  (begin-example "2.21")
  (displn-eval '(square-list (iota 6)) ns)
  (displn-eval '(square-list-2 (iota 6)) ns))


;; Exercise 2.22

;; Louis's first answer reverses because it conses elements beginning
;; from the front of the original list building onto an empty list

;; His second answer is wrong because to build a list of elements of a
;; given type with cons we need to cons an element onto a list each
;; time whereas the new function conses a list onto an element.

(define (for-each-2 proc items)
  (if (null? items)
      #t
      ((lambda (proc items hunoz)
         (for-each-2 proc items))
       proc (cdr items) (proc (car items)))))

(module+ main
  (begin-example "2.23")
  (displn-eval '(for-each-2 displn (iota 6)) ns))


