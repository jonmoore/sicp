#lang sicp
(#%require
 racket
 rackunit
 "../utils/sicp-utils.rkt"
 "../utils/srfi-1.rkt"
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex-1.11

(define (f3sum f1 f2 f3)
  (+ (* 1 f1) (* 2 f2) (* 3 f3)))

(define (fib3-rec n)
  (if (< n 3)
      n
      (f3sum
       (fib3-rec (- n 1))
       (fib3-rec (- n 2))
       (fib3-rec (- n 3)))))

(define (fib3-iter n)
  (define (iter i fi-1 fi-2 fi-3)
    (define fi (f3sum fi-1 fi-2 fi-3))
    (if (= i n)
        fi
        (iter (+ i 1) fi fi-1 fi-2)))
  (if (< n 3) 
      n
      (iter 3 2 1 0)))

(module+ main
  (begin-example "1.11")
  (displayln "The fib3 series")
  (for-each
   (lambda (n)
     (display (list n (fib3-iter n)))
     (display " "))
   (iota 6))
  (displayln "..."))

(module+ test
  (test-equal?
   "fib 3 equivalence of recursive vs iterative procedures."
   (map fib3-rec (iota 8))
   (map fib3-iter (iota 8))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex-1.12

(define (pas row col)
  "row and col are indexed from 0" 
  (cond
    ((< row 0) (error "negative row" row))
    ((< col 0) (error "negative col" col))
    ((> col row) (error " col > row" col row))
    ((= col row) 1)
    ((= col 0) 1)
    (else (+ (pas (- row 1) col)
             (pas (- row 1) (- col 1))))))

(define (from-to a b)
  (iota (max 0 (- b a))
        a))

(define (pas-row n)
  (map
   (lambda (col)
     (pas n col))
   (iota (inc n))))

(define (pas-tri n)
  (map pas-row
       (iota (inc n))))

(module+ main
  (begin-example "1.12")
  (displayln "A Pascal triangle")
  (pas-tri 6))
