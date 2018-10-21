#lang sicp
(#%require (only racket module+ define-namespace-anchor namespace-anchor->namespace) rackunit racket/trace)
(#%require "../utils/sicp-utils.rkt" "../utils/srfi-1.rkt")
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(define (make-dpf division file)
  (list division file))

(define (get op division)
  (error "get not implemented"))

(define (dpf-division pf) (car pf))
(define (dpf-file pf) (cadr pf))

(define (get-record employee-name dpf)
  (let* ((division (dpf-division dpf))
         (getter (get 'get-record division)))
    (getter employee-name (dpf-file dpf))))

(define (get-salary employee-name dpf)
  (let* ((record (get-record employee-name dpf))
         (division (dpf-division dpf))
         (getter (get 'get-salary division)))
    (getter record)))

(define (find-employee-record employee-name dpfs)
  ;; assume get-record returns #f no record is found
  (any (lambda (dpf) (get-record employee-name dpf)) dpfs))

;; have to put in the functions matching the get calls above and
;; ensure they are consistent with our usage and assumptions, by
;; wrapping if needed.
