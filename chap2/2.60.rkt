#lang sicp
(#%require (only racket module+ define-namespace-anchor namespace-anchor->namespace) rackunit racket/trace)
(#%require "../utils/sicp-utils.rkt" "../utils/srfi-1.rkt")
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

;; Sets as unordered lists, allowing duplicates

;; These stay the same
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (cond ((null? set1) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;; These are simplified

(define (adjoin-set x set)
  (cons x set))

(define (union-set set1 set2)
  (append set1 set2))


(module+ main
  (begin-example "2.60")
  (displn-eval '(union-set '(1 2 3) '(1 2 4)) ns))

;; Performance.  Adjoining or unioning are faster because we don't
;; have to check for membership as we perform these. Testing for
;; membership, and thus intersection, are slower because we generally
;; have to deal with lists that have duplicates.  Would tend to use
;; the representation with duplicates when duplicates are rare or when
;; we need relatively few tests of membership compared to additions of
;; members.

