#lang sicp
(#%require (only racket module+ define-namespace-anchor namespace-anchor->namespace) rackunit)
(#%require "../utils/sicp-utils.rkt" "../utils/srfi-1.rkt")
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(define (square-tree-direct items)
  (define (process-car car-from)
    (if (atom? car-from)
        (square car-from)
        (square-tree-direct car-from)))
  (if (null? items)
      nil
      (cons (process-car (car items))
            (square-tree-direct (cdr items)))))

(define (square-tree-map items)
  (if (atom? items)
      (square items)
      (map square-tree-map items)))

(module+ main
  (begin-example "2.30")
  (displn-eval '(square-tree-direct (list 1
                                     (list 2 (list 3 4) 5)
                                     (list 6 7))) ns)
  (displn-eval '(square-tree-map (list 1
                                       (list 2 (list 3 4) 5)
                                       (list 6 7))) ns))

(define (tree-map func items)
  (if (atom? items)
      (func items)
      (map (lambda (item) (tree-map func item)) items)))

(define (square-tree tree) (tree-map square tree))

(module+ main
  (begin-example "2.31")
  (displn-eval '(square-tree (list 1
                                   (list 2 (list 3 4) 5)
                                   (list 6 7))) ns))


;; 2.32

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest
                (map (lambda (ss)
                       (cons (car s) ss))
                     rest)))))

(module+ main
        (begin-example "2.32")
        (displn-eval '(subsets (list 1 2 3)) ns))

;; Why it works. By assumption all the elements in the list are
;; distinct.  For a non-empty list the subsets thus divide cleanly
;; into those that do and do not include the first element in the
;; list, generating two subsets for each subset from the rest of the
;; elements.  Termination is covered by the handling of the empty
;; list.
