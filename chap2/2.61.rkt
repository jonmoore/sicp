#lang sicp
(#%require (only racket module+ define-namespace-anchor namespace-anchor->namespace) rackunit racket/trace)
(#%require "../utils/sicp-utils.rkt" "../utils/srfi-1.rkt")
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

;; Sets as ordered lists

;; These stay the same
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if ((or (null? set1) (null? set2))
       '())
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set (cdr set1) (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

;; 2.61
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else
         (cons (car set) (adjoin-set x (cdr set))))))

;; 2.62
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1))
               (x2 (car set2)))
           (cond ((= x1 x2)
                  (cons x1 (union-set (cdr set1) (cdr set2))))
                 ((< x1 x2)
                  (cons x1 (union-set (cdr set1) set2)))
                 ((< x2 x1)
                  (cons x2 (union-set set1 (cdr set2)))))))))

(module+ main
  (begin-example "2.61")
  (displn-eval '(adjoin-set 0 '(1 2 4)) ns)
  (displn-eval '(adjoin-set 1 '(1 2 4)) ns)
  (displn-eval '(adjoin-set 3 '(1 2 4)) ns)
  (displn-eval '(adjoin-set 5 '(1 2 4)) ns)
  (begin-example "2.62")
  (displn-eval '(union-set '(1 2 3) '(1 2 4)) ns))

