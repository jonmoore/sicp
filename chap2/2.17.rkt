#lang sicp
(#%require (only racket module+ define-namespace-anchor namespace-anchor->namespace) rackunit)
(#%require "../utils/sicp-utils.rkt" "../utils/srfi-1.rkt")

(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(define (last-pair lis)
  (if (null? (cdr lis))
      lis
      (last-pair (cdr lis))))

(module+ main
  (begin-example "2.17")
  (displn-eval '(last-pair (list 23 73 149 34)) ns))


(define (reverse lis)
  (define (iter from to)
    (if (null? from)
        to
        (iter (cdr from) (cons (car from) to))))
  (iter lis nil))

(module+ main
  (begin-example "2.18")
  (displn-eval '(reverse (list 1 4 9 16 25)) ns))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount (except-first-denomination coin-values))
            (cc (- amount (first-denomination coin-values))
                coin-values)))))

(define no-more? null?)
(define first-denomination car)
(define except-first-denomination cdr)

(define us-coins (list 50 25 10 5 1))
(define us-coins-reversed (reverse us-coins))
(define uk-coins (list 100 50 20 10 5 2 1))

(module+ main
  (begin-example "2.19")
  (displn-eval '(cc 100 us-coins) ns)
  (displn-eval '(cc 100 us-coins-reversed) ns)
  (displn-eval '(cc 100 uk-coins) ns))

;; answer does not depend on order since the spec does not and we've
;; coded to meet the spec.

(define (same-parity first . rest)
  (define (matches el)
    (= (modulo el 2) (modulo first 2)))
  (define (matching rest)
    (if (null? rest) rest
        (if (matches (car rest))
            (cons (car rest) (matching (cdr rest)))
            (matching (cdr rest)))))
  (cons first (matching rest)))

(module+ main
  (begin-example "2.20")
  (displn-eval '(same-parity 1 3 4 5 6 7) ns)
  (displn-eval '(same-parity 2 3 4 5 6 7) ns))
